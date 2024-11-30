{-# LANGUAGE RecordWildCards #-}

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
  nameMap <- getNameMapFile >>= atomically <$> newTMVar
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

getNameMap :: TMVar NameMap -> IO NameMap
getNameMap = atomically . readTMVar

updateNameMap :: TMVar NameMap -> (NameMap -> NameMap) -> IO ()
updateNameMap nameMap f =
  atomically
    ( do
        m <- takeTMVar nameMap
        let new = f m
        putTMVar nameMap new
        pure new
    )
    >>= void . forkIO . setNameMapFile

getNameMapFile :: IO NameMap
getNameMapFile =
  (try @IOException $ read . decodeUtf8 <$> readFileBS "./name_map") <&> \case
    Right m -> m
    Left _ -> M.empty

setNameMapFile :: NameMap -> IO ()
setNameMapFile m =
  try @IOException (writeFileBS "./name_map" (encodeUtf8 @Text @ByteString $ show m)) >>= \case
    Left i -> print i
    Right () -> pass

handler :: TMVar NameMap -> Event -> DiscordHandler ()
handler nameMap = \case
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
            ( do
                uid <- case memberOrUser of
                  Right user -> pure $ userId user
                  Left (GuildMember {memberUser = Just user}) -> pure $ userId user
                  _ -> die "no user"
                gid <- case gid' of
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
                    rolledName <- Response $ lift $ lift $ do
                      m <- getNameMap nameMap
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
                            nop {modifyGuildMemberOptsNickname = Just rolledName}
                    respond $ interactionResponseBasic $ "<:cap:1307053757351333929>" <> rolledName <> "<:cap:1307053757351333929>"
                  "n" -> do
                    newName <- getNameArg
                    Response $
                      lift $
                        rc_ $
                          ModifyGuildMember
                            gid
                            uid
                            nop {modifyGuildMemberOptsNickname = Just newName}
                    respond $ interactionResponseBasic "okay <:cap:1307053757351333929>"
                  "add" -> do
                    putStrLn "running add"
                    newName <- getNameArg
                    Response $
                      lift $
                        lift $
                          updateNameMap nameMap $
                            M.alter (Just . (newName :) . fromMaybe []) uid
                    respond $ interactionResponseBasic "Added"
                    putStrLn "done add"
                  "rm" -> do
                    putStrLn "running rm"
                    targetName <- getNameArg
                    Response $
                      lift $
                        lift $
                          updateNameMap nameMap $
                            M.alter (Just . filter (/= targetName) . fromMaybe []) uid
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
              nameMap' <- getNameMap nameMap
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

nop :: ModifyGuildMemberOpts
nop =
  ModifyGuildMemberOpts
    { modifyGuildMemberOptsNickname = Nothing
    , modifyGuildMemberOptsRoles = Nothing
    , modifyGuildMemberOptsIsMuted = Nothing
    , modifyGuildMemberOptsIsDeafened = Nothing
    , modifyGuildMemberOptsMoveToChannel = Nothing
    , modifyGuildMemberOptsTimeoutUntil = Nothing
    }
