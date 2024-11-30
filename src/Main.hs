{-# LANGUAGE RecordWildCards #-}

module Main where

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
  print
    =<< runDiscord
      def
        { discordToken = token
        , discordOnStart = liftIO $ putStrLn "Starting"
        , discordOnEvent = handler
        , discordGatewayIntent =
            def
              { gatewayIntentMembers = True
              , gatewayIntentPresences = True
              }
        }

getNameMap :: IO (Map UserId [Text])
getNameMap =
  (try @IOException $ read . decodeUtf8 <$> readFileBS "./name_map") <&> \case
    Right m -> m
    Left _ -> M.empty

setNameMap :: Map UserId [Text] -> IO ()
setNameMap m =
  try @IOException (writeFileBS "./name_map" (encodeUtf8 @Text @ByteString $ show m)) >>= \case
    Left i -> print i
    Right () -> pass

handler :: Event -> DiscordHandler ()
handler = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    putStrLn "ready"
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs =
          Prelude.filter
            (\c -> applicationCommandName c `notElem` (createName <$> coms))
            oldComs
    putStrLn "removing coms"
    print removedComs
    forM_ removedComs $ rc_ . DeleteGlobalApplicationCommand i . applicationCommandId
    putStrLn "oldComs"
    print oldComs
    putStrLn "registering commands"
    forM_
      coms
      ( \c -> do
          print c
          rc $ CreateGlobalApplicationCommand i c
      )
    putStrLn "commands registered"
  InteractionCreate interaction ->
    mkInteractionHandler interaction $
      case interaction of
        ( InteractionApplicationCommand
            { applicationCommandData =
              ApplicationCommandDataChatInput
                { applicationCommandDataName = name
                , optionsData = options
                }
            , interactionUser = MemberOrUser memberOrUser
            , interactionGuildId = gid'
            }
          ) ->
            ( case name of
                "help" ->
                  respond $ interactionResponseBasic "<@283006687412224001> write some help text nerd"
                "rn" -> do
                  putStrLn "running rn"
                  uid <- case memberOrUser of
                    Right user -> pure $ userId user
                    Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                    _ -> die "no user"
                  let curName = case memberOrUser of
                        Left (GuildMember {memberNick = (Just nick)}) -> nick
                        _ -> ""
                  gid <- case gid' of
                    Just gid -> pure gid
                    Nothing -> die "no gid"
                  rolledName <- Response $ lift $ lift $ do
                    m <- getNameMap
                    print m
                    putStrLn "read map"
                    putStrLn "excludes"
                    print curName
                    putStrLn "choices"
                    let names = filter (/= curName) $ fromMaybe [] $ M.lookup uid m
                    print names
                    i <- randomRIO (0, length names - 1)
                    pure $ names !! i
                  Response $
                    lift $
                      rc_ $
                        ModifyGuildMember
                          gid
                          uid
                          ModifyGuildMemberOpts
                            { modifyGuildMemberOptsNickname = Just rolledName
                            , modifyGuildMemberOptsRoles = Nothing
                            , modifyGuildMemberOptsIsMuted = Nothing
                            , modifyGuildMemberOptsIsDeafened = Nothing
                            , modifyGuildMemberOptsMoveToChannel = Nothing
                            , modifyGuildMemberOptsTimeoutUntil = Nothing
                            }
                  respond $ interactionResponseBasic "okay <:cap:1307053757351333929>"
                "n" -> do
                  putStrLn "running n"
                  respond $ interactionResponseBasic "okay <:cap:1307053757351333929>"
                  uid <- case memberOrUser of
                    Right user -> pure $ userId user
                    Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                    _ -> die "no user"
                  gid <- case gid' of
                    Just gid -> pure gid
                    Nothing -> die "no gid"
                  newName <- case options of
                    Just (OptionsDataValues [OptionDataValueString {optionDataValueString = Right val}]) ->
                      pure val
                    _ -> die "bad options"
                  Response $
                    lift $
                      rc_ $
                        ModifyGuildMember
                          gid
                          uid
                          ModifyGuildMemberOpts
                            { modifyGuildMemberOptsNickname = Just newName
                            , modifyGuildMemberOptsRoles = Nothing
                            , modifyGuildMemberOptsIsMuted = Nothing
                            , modifyGuildMemberOptsIsDeafened = Nothing
                            , modifyGuildMemberOptsMoveToChannel = Nothing
                            , modifyGuildMemberOptsTimeoutUntil = Nothing
                            }
                "add" -> do
                  putStrLn "running add"
                  uid <- case memberOrUser of
                    Right user -> pure $ userId user
                    Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                    _ -> die "no user"
                  newName <- case options of
                    Just (OptionsDataValues [OptionDataValueString {optionDataValueString = Right val}]) ->
                      pure val
                    _ -> die "bad options"
                  Response $ lift $ lift $ do
                    m <- getNameMap
                    print m
                    putStrLn "read map"
                    let names = fromMaybe [] $ M.lookup uid m
                    setNameMap $ M.insert uid (newName : names) m
                  respond $ interactionResponseBasic "Added"
                  putStrLn "done add"
                "rm" -> do
                  putStrLn "running rm"
                  uid <- case memberOrUser of
                    Right user -> pure $ userId user
                    Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                    _ -> die "no user"
                  newName <- case options of
                    Just (OptionsDataValues [OptionDataValueString {optionDataValueString = Right val}]) ->
                      pure val
                    _ -> die "bad options"
                  Response $ lift $ lift $ do
                    m <- getNameMap
                    let names = fromMaybe [] $ M.lookup uid m
                    setNameMap $ M.insert uid (filter (/= newName) names) m
                  respond $ interactionResponseBasic "Removed"
                c -> print c
            )
        ( InteractionApplicationCommandAutocomplete
            { applicationCommandData =
              ApplicationCommandDataChatInput {}
            , interactionUser = MemberOrUser memberOrUser
            }
          ) -> do
            names <- Response $ lift $ lift $ do
              uid <- case memberOrUser of
                Right user -> pure $ userId user
                Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                _ -> die "no user"
              nameMap <- getNameMap
              pure $ fromMaybe [] . M.lookup uid $ nameMap
            respond $ do
              InteractionResponseAutocompleteResult $
                InteractionResponseAutocompleteString $
                  (\n -> Choice {choiceName = n, choiceValue = n, choiceLocalizedName = Nothing})
                    <$> names
        i -> pass
  e -> pass

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
              , optionValueStringChoices = Left True
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

newtype Response a
  = Response
      (ReaderT Info DiscordHandler a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

data Info = Info
  { infoInteractionId :: InteractionId
  , infoInteractionToken :: InteractionToken
  , infoInteractionApplicationId :: ApplicationId
  }

respond :: InteractionResponse -> Response ()
respond ir = Response $ do
  Info {..} <- ask
  lift $
    rc_ $
      CreateInteractionResponse
        infoInteractionId
        infoInteractionToken
        ir

mkInteractionHandler :: Interaction -> Response () -> DiscordHandler ()
mkInteractionHandler interaction (Response r) =
  runReaderT
    r
    Info
      { infoInteractionId = interactionId interaction
      , infoInteractionToken = interactionToken interaction
      , infoInteractionApplicationId = interactionApplicationId interaction
      }
