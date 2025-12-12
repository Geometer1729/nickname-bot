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
import Discord.Internal.Rest.Channel (ChannelRequest (..), StartThreadOpts (..))
import Discord.Internal.Rest.Guild
import Discord.Internal.Rest.Interactions
import Discord.Requests (MessageDetailedOpts (..))
import Relude.Unsafe (read, (!!))
import System.Directory (removeFile)
import System.INotify (EventVariety (Create), addWatch, initINotify)
import System.INotify qualified as INotify
import System.Random

main :: IO ()
main = do
  token <-
    getArgs >>= \case
      [] -> T.strip . decodeUtf8 <$> readFileBS "./token.auth"
      [token] -> pure $ T.strip $ fromString token
      _ -> die "Expected 0 or 1 arguments"
  dontPingFor <- newTVarIO @_ @[Text] []
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
        , discordOnEvent = handler dontPingFor nameMap
        , discordOnEnd = flushMap nameMap
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
  void $ forkIO $ flushMap nameMap

flushMap :: TVar NameMap -> IO ()
flushMap nameMap = do
  m <- readTVarIO nameMap
  try @IOException (writeFileBS "./name_map" (encodeUtf8 @Text @ByteString $ show m)) >>= \case
    Left i -> print i
    Right () -> pass

handler :: TVar [Text] -> TVar NameMap -> Event -> DiscordHandler ()
handler dontPingFor nameMap = \case
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    ino <- liftIO initINotify
    h <- ask
    void $ liftIO $ addWatch ino [Create] "/data/discord-bots/nickname-bot" $ \case
      INotify.Created False _ -> do
        putStrLn "Triggered"
        t <- T.strip . decodeUtf8 <$> readFileBS "/data/discord-bots/nickname-bot/report"
        print t
        (`runReaderT` h) $
          rc_ $
            CreateMessage (DiscordId $ Snowflake 1319497196760207400) $
              "<@283006687412224001> Merge report: " <> t
        removeFile "/data/discord-bots/nickname-bot/report"
      _ -> pass
    putStrLn "ready"
    sendCommand $
      UpdateStatus $
        UpdateStatusOpts
          { updateStatusOptsSince = Nothing
          , updateStatusOptsActivities = [mkActivity "you all for fools" ActivityTypeGame]
          , updateStatusOptsNewStatus = UpdateStatusOnline
          , updateStatusOptsAFK = False
          }
    oldComs <- rc $ GetGlobalApplicationCommands i
    let removedComs =
          filter
            (\c -> applicationCommandName c `notElem` (createName <$> coms))
            oldComs
    forM_ removedComs $ rc . DeleteGlobalApplicationCommand i . applicationCommandId
    forM_ coms $ rc . CreateGlobalApplicationCommand i
    putStrLn "commands registered"
  -- MessageCreate m@Message{messageChannelId = DiscordId (Snowflake 1319497196760207400)} -> do
  MessageCreate m@Message {messageChannelId = DiscordId (Snowflake 1319497196760207400)} -> do
    -- Create thread for messages in specific channel
    let threadName = "Thread: " <> T.take 50 (messageContent m)
        opts =
          StartThreadOpts
            { startThreadName = threadName
            , startThreadAutoArchive = Just 60
            , startThreadRateLimit = Nothing
            }
    putStrLn "DEBUG"
    result <- restCall $ StartThreadFromMessage (messageChannelId m) (messageId m) opts
    case result of
      Left err -> putStrLn $ "Failed to create thread: " <> show err
      Right thread -> do
        putStrLn $ "Created thread for message: " <> show (messageId m)
        -- Post "Answers here:" message in the thread
        void $ restCall $ CreateMessage (channelId thread) "Answers here:"
  GuildMemberUpdate gid _ user (Just newNickname) -> do
    let uid = userId user
    m <- readTVarIO nameMap
    let names = fromMaybe [] $ M.lookup uid m
    let mcid = case gid of
          (DiscordId (Snowflake 1189715747723817010)) -> Just $ DiscordId $ Snowflake 1312176640557715476
          _ -> Nothing
    -- To avoid asking them (twice) on `/n`s
    dontPingForNames <- readTVarIO dontPingFor
    print dontPingForNames
    unless (newNickname `elem` names) $
      if newNickname `elem` dontPingForNames
        then lift $ atomically $ modifyTVar' dontPingFor (filter (/= newNickname))
        else forM_ mcid $ \cid ->
          rc_ $
            CreateMessageDetailed
              cid
              MessageDetailedOpts
                { messageDetailedContent = "<@" <> show uid <> "> Want to add \"" <> newNickname <> "\" as a favorite?"
                , messageDetailedTTS = False
                , messageDetailedEmbeds = Nothing
                , messageDetailedFile = Nothing
                , messageDetailedStickerIds = Nothing
                , messageDetailedAllowedMentions = Nothing
                , messageDetailedReference = Nothing
                , messageDetailedComponents =
                    Just
                      [ ActionRowButtons
                          [ Button
                              { buttonCustomId = show uid <> ":" <> newNickname
                              , buttonDisabled = False
                              , buttonStyle = ButtonStylePrimary
                              , buttonLabel = Just "Yes"
                              , buttonEmoji = Nothing
                              }
                          ]
                      ]
                }
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
                    respond $ interactionResponseBasic "The main commands you probably want to look at are /rn,/n and /add.\n If there's an issue with the bot ping me."
                  "rn" -> do
                    let curName = case memberOrUser of
                          Left (GuildMember {memberNick = (Just nick)}) -> nick
                          _ -> ""
                    rolledName <- liftIO $ do
                      m <- readTVarIO nameMap
                      let names = filter (/= curName) $ fromMaybe [] $ M.lookup uid m
                      case names of
                        [] -> pure "Add a nickname to use /rn"
                        _ -> do
                          i <- randomRIO (0, length names - 1)
                          pure $ names !! i
                    setName gid uid rolledName
                    respond $ interactionResponseBasic $ cap <> rolledName <> cap
                  "n" -> do
                    newName <- getNameArg
                    m <- readTVarIO nameMap
                    let names = fromMaybe [] $ M.lookup uid m
                    if newName `elem` names
                      then do
                        setName gid uid newName
                        respond $ interactionResponseBasic $ "Nicked" <> cap
                      else do
                        lift $ atomically $ modifyTVar' dontPingFor (newName :)
                        setName gid uid newName
                        respond $
                          InteractionResponseChannelMessage $
                            InteractionResponseMessage
                              { interactionResponseMessageTTS = Nothing
                              , interactionResponseMessageContent =
                                  Just $ "Nicked to " <> newName <> cap
                              , interactionResponseMessageEmbeds = Nothing
                              , interactionResponseMessageAllowedMentions = Nothing
                              , interactionResponseMessageFlags = Nothing
                              , interactionResponseMessageComponents =
                                  Just
                                    [ ActionRowButtons
                                        [ Button
                                            { buttonCustomId = show uid <> ":" <> newName
                                            , buttonDisabled = False
                                            , buttonStyle = ButtonStylePrimary
                                            , buttonLabel = Just "Add"
                                            , buttonEmoji = Nothing
                                            }
                                        ]
                                    ]
                              , interactionResponseMessageAttachments = Nothing
                              }
                  "add" -> do
                    newName <- getNameArg
                    liftIO $
                      updateNameMap nameMap $
                        M.alter (Just . (newName :) . fromMaybe []) uid
                    respond $ interactionResponseBasic "Added"
                  "rm" -> do
                    targetName <- getNameArg
                    liftIO $
                      updateNameMap nameMap $
                        M.alter (Just . filter (/= targetName) . fromMaybe []) uid
                    respond $ interactionResponseBasic "Removed"
                  "restart" -> do
                    sendCommand $
                      UpdateStatus $
                        UpdateStatusOpts
                          { updateStatusOptsSince = Nothing
                          , updateStatusOptsActivities = [mkActivity "restarting" ActivityTypeGame]
                          , updateStatusOptsNewStatus = UpdateStatusDoNotDisturb
                          , updateStatusOptsAFK = True
                          }
                    respond $ interactionResponseBasic "Bye"
                    stopDiscord
                  c -> print c
            )
        (InteractionComponent {componentData = ButtonData button}) -> do
          let (uid', T.stripPrefix ":" -> mname) = T.breakOn ":" button
          let uid = read @UserId $ toString uid'
          newName <- maybe (die "bad button id?") pure mname
          liftIO $
            updateNameMap nameMap $
              M.alter (Just . (newName :) . fromMaybe []) uid
          respond $
            InteractionResponseUpdateMessage $
              InteractionResponseMessage
                { interactionResponseMessageTTS = Nothing
                , interactionResponseMessageContent = Nothing
                , interactionResponseMessageEmbeds = Nothing
                , interactionResponseMessageAllowedMentions = Nothing
                , interactionResponseMessageFlags = Nothing
                , interactionResponseMessageComponents =
                    Just
                      [ ActionRowButtons
                          [ Button
                              { buttonCustomId = show uid <> ":" <> newName
                              , buttonDisabled = True
                              , buttonStyle = ButtonStylePrimary
                              , buttonLabel = Just "Add"
                              , buttonEmoji = Nothing
                              }
                          ]
                      ]
                , interactionResponseMessageAttachments = Nothing
                }
        ( InteractionApplicationCommandAutocomplete
            { applicationCommandData =
              ApplicationCommandDataChatInput
                { applicationCommandDataName = commandName
                , optionsData =
                  Just
                    ( OptionsDataValues
                        [OptionDataValueString {optionDataValueString = Left w}]
                      )
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
                  let names = fromMaybe [] . M.lookup uid $ nameMap'
                  pure $
                    sortOn
                      ( Down
                          . ( \name ->
                                length $
                                  takeWhile (uncurry (==)) $
                                    T.zip (T.toLower name) (T.toLower w)
                            )
                      )
                      names
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
  , simpleCommand "restart" "restart the bot" Nothing
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
