module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try)
import Control.Exception.Base (IOException)
import Data.Map.Lazy qualified as M
import Data.Text qualified as T
import Discord
import Discord.Interactions
import Discord.Internal.Rest
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Rest.Guild
import Discord.Internal.Rest.Interactions
import Relude.Unsafe (read, (!!))
import System.Random

main :: IO ()
main = do
  token <-
    getArgs >>= \case
      [] -> T.strip . decodeUtf8 <$> readFileBS "./token.auth"
      [token] -> pure $ T.strip $ fromString token
      _ -> die "Expected 0 or 1 arguments"
  nameMap <-
    try @IOException (read . decodeUtf8 <$> readFileBS "./name_map") <&> \case
      Right m -> m
      Left _ -> M.empty
      >>= newTVarIO
  print
    =<< runDiscord
      def
        { discordToken = token
        , discordOnStart = liftIO $ putStrLn "Starting"
        , discordOnEvent = handler nameMap
        , discordGatewayIntent =
            def
              { gatewayIntentMembers = True
              , gatewayIntentPresences = True
              }
        }

type NameMap = Map UserId [Text]

updateNameMap :: TVar NameMap -> (NameMap -> NameMap) -> IO ()
updateNameMap nameMap f = do
  atomically $ modifyTVar' nameMap f
  void $ forkIO $ do
    m <- readTVarIO nameMap
    try @IOException (writeFileBS "./name_map" (encodeUtf8 @Text @ByteString $ show m)) >>= \case
      Left i -> print i
      Right () -> pass

handler :: TVar NameMap -> Event -> DiscordHandler ()
handler nameMap = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs =
          filter
            (\c -> applicationCommandName c `notElem` (createName <$> coms))
            oldComs
    forM_ removedComs $ rc . DeleteGlobalApplicationCommand i . applicationCommandId
    forM_ coms $ rc . CreateGlobalApplicationCommand i
    putStrLn "commands registered"
  InteractionCreate interaction ->
    withResponder interaction $ \respond ->
      case interaction of
        ( InteractionApplicationCommand
            { applicationCommandData =
              ApplicationCommandDataChatInput
                { applicationCommandDataName = name
                , optionsData = options
                }
            , interactionUser = MemberOrUser memberOrUser
            , interactionGuildId = mgid
            }
          ) ->
            ( do
                uid <- case memberOrUser of
                  Right user -> pure $ userId user
                  Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                  _ -> die "no user"
                gid <- case mgid of
                  Just gid -> pure gid
                  Nothing -> die "no gid"
                let getNameArg = maybe (die "bad args") pure $ case options of
                      Just (OptionsDataValues [OptionDataValueString {optionDataValueString = Right val}]) ->
                        Just val
                      _ -> Nothing
                case name of
                  "help" ->
                    respond $ interactionResponseBasic "Try adding some names with /add and then use /rn to roll a name"
                  "rn" -> do
                    let curName = case memberOrUser of
                          Left (GuildMember {memberNick = (Just nick)}) -> nick
                          _ -> ""
                    rolledName <- liftIO $ do
                      m <- readTVarIO nameMap
                      let names = filter (/= curName) $ fromMaybe [] $ M.lookup uid m
                      i <- randomRIO (0, length names - 1)
                      pure $ names !! i
                    setName gid uid rolledName
                    respond $ interactionResponseBasic $ cap <> rolledName <> cap
                  "n" -> do
                    getNameArg >>= setName gid uid
                    respond $ interactionResponseBasic $ "Nicked" <> cap
                  "add" -> do
                    newName <- getNameArg
                    liftIO $
                      updateNameMap nameMap $
                        M.alter (Just . (newName :) . fromMaybe []) uid
                    respond $ interactionResponseBasic "Added"
                  "rm" -> do
                    putStrLn "running rm"
                    targetName <- getNameArg
                    liftIO $
                      updateNameMap nameMap $
                        M.alter (Just . filter (/= targetName) . fromMaybe []) uid
                    respond $ interactionResponseBasic "Removed"
                  c -> print c
            )
        ( InteractionApplicationCommandAutocomplete
            { applicationCommandData =
              ApplicationCommandDataChatInput
                { applicationCommandDataName = commandName
                }
            , interactionUser = MemberOrUser memberOrUser
            }
          ) ->
            do
              names <- case commandName of
                "add" -> pure []
                _ -> liftIO $ do
                  uid <- case memberOrUser of
                    Right user -> pure $ userId user
                    Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                    _ -> die "no user"
                  nameMap' <- readTVarIO nameMap
                  pure $ fromMaybe [] . M.lookup uid $ nameMap'
              respond $ do
                InteractionResponseAutocompleteResult $
                  InteractionResponseAutocompleteString $
                    (\n -> Choice {choiceName = n, choiceValue = n, choiceLocalizedName = Nothing})
                      <$> names
        _i -> pass
  _e -> pass

coms :: [CreateApplicationCommand]
coms =
  [ simpleCommand "n" "sets nickname" $
      Just $
        OptionsValues
          [ OptionValueString
              { optionValueName = "name"
              , optionValueLocalizedName = Nothing
              , optionValueDescription = "The name to nick"
              , optionValueLocalizedDescription = Nothing
              , optionValueStringMinLen = Nothing
              , optionValueStringMaxLen = Nothing
              , optionValueStringChoices = Left True
              , optionValueRequired = True
              }
          ]
  , simpleCommand "add" "adds nickname" $
      Just $
        OptionsValues
          [ OptionValueString
              { optionValueName = "name"
              , optionValueLocalizedName = Nothing
              , optionValueDescription = "The name to nick"
              , optionValueLocalizedDescription = Nothing
              , optionValueStringMinLen = Nothing
              , optionValueStringMaxLen = Nothing
              , optionValueStringChoices = Left False
              , optionValueRequired = True
              }
          ]
  , simpleCommand "rm" "rms nickname" $
      Just $
        OptionsValues
          [ OptionValueString
              { optionValueName = "name"
              , optionValueLocalizedName = Nothing
              , optionValueDescription = "The name to nick"
              , optionValueLocalizedDescription = Nothing
              , optionValueStringMinLen = Nothing
              , optionValueStringMaxLen = Nothing
              , optionValueStringChoices = Left True
              , optionValueRequired = True
              }
          ]
  , simpleCommand "rn" "chose random name" Nothing
  , simpleCommand "help" "send help text" Nothing
  ]

simpleCommand :: Text -> Text -> Maybe Options -> CreateApplicationCommand
simpleCommand name desc opts =
  CreateApplicationCommandChatInput
    { createName = name
    , createLocalizedName = Nothing
    , createDescription = desc
    , createLocalizedDescription = Nothing
    , createOptions = opts
    , createDefaultMemberPermissions = Nothing
    , createDMPermission = Nothing
    }

rc_ :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
rc_ = void . rc

rc :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
rc a =
  restCall a >>= \case
    Right r -> pure r
    Left err -> die $ show err

cap :: Text
cap = "<:cap:1307053757351333929>"

withResponder :: Interaction -> ((InteractionResponse -> DiscordHandler ()) -> DiscordHandler a) -> DiscordHandler a
withResponder interaction f = f $ rc . CreateInteractionResponse (interactionId interaction) (interactionToken interaction)

setName :: GuildId -> UserId -> Text -> DiscordHandler ()
setName gid uid name =
  rc_ $
    ModifyGuildMember
      gid
      uid
      ModifyGuildMemberOpts
        { modifyGuildMemberOptsNickname = Just name
        , modifyGuildMemberOptsRoles = Nothing
        , modifyGuildMemberOptsIsMuted = Nothing
        , modifyGuildMemberOptsIsDeafened = Nothing
        , modifyGuildMemberOptsMoveToChannel = Nothing
        , modifyGuildMemberOptsTimeoutUntil = Nothing
        }
