module Royo.Bot where

import Prelude hiding (between)

import Control.Alt (alt)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, drop, length, stripPrefix, stripSuffix, trim)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.Utils (startsWith)
import Discord.DiscordJS (DiscordToken, Message, createDMChannel, login, newClient, offMessageUpdate, onMessage, onMessageUpdate, react, sendString)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, error, forkAff, killFiber, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Effect
import Yoga.APIClient (CompileResult, YogaToken, compileAndRun)

_BOT_USER_ID ∷ String
_BOT_USER_ID = "625255925627748363"

mentionPrefix1 ∷ String
mentionPrefix1 = i "<@!" _BOT_USER_ID ">"

_OTHER_BOT_USER_ID ∷ String
_OTHER_BOT_USER_ID = "625255925627748363"

mentionPrefix2 ∷ String
mentionPrefix2 = i "<@&" _OTHER_BOT_USER_ID ">"

printErrors :: Message -> Aff Unit -> Aff Unit
printErrors msg aff = do 
  result <- attempt aff
  case result of 
    Left e -> log $ "Couldnt' handle message" <>  message e <> "\n" <> msg.content
    Right _ -> pure unit

runBot ∷ YogaToken -> DiscordToken -> Aff Unit
runBot yogaToken discordToken = do
  newClient <- newClient # liftEffect
  client <- newClient # login discordToken
  client # onMessage (launchAff_ <$> messageHandler client) # liftEffect
  where
    messageHandler client msg@{ content } = printErrors msg do
      when (isMention content) do
        parseAndRun msg

        -- Create a countdown
        stopFR <- do
          stopF <- forkAff stopSession
          Effect.new stopF # liftEffect

        let recvMessage old new =
              when (msg.id == old.id && msg.id == new.id) do
                -- Kill countdown
                stopF_ <- Effect.read stopFR # liftEffect
                killFiber (error "messageHandler: reset timer") stopF_

                -- Renew countdown
                stopF <- forkAff stopSession
                Effect.write stopF stopFR # liftEffect

                parseAndRun new

        -- Register to messageUpdate
        client # onMessageUpdate (launchAff_ `map map map` recvMessage) # liftEffect

        where
          -- Unregister from messageUpdate
          stopSession = do
            delay (Milliseconds 10_000.0)
            client # offMessageUpdate # liftEffect

    parseAndRun msg@{ channel, content } = do
      case parseCodeBlock content of
        Nothing -> do
          sendBasicInstructions msg
        Just code -> do
          res <- compileAndRun yogaToken { code: prepareCode code }
          case res of
            Left cr ->
              sendCompileProblem msg code cr
            Right rr ->
              case NES.fromString rr.stdout, NES.fromString rr.stderr of
                Just _, _ -> do
                  msg # react robotMuscleEmoji
                  channel # sendString (NonEmptyString (prepareOutput rr.stdout))
                Nothing, Just _ -> do
                  msg # react sirenEmoji
                  sendRunProblem msg code rr.stderr
                _, _ -> channel # sendString (NonEmptyString "Something is weird")


sendBasicInstructions :: Message -> Aff Unit 
sendBasicInstructions msg = do
  msg # react thinkingEmoji
  dmc <- createDMChannel msg.author
  dmc # sendString (NonEmptyString intro)
  where
  intro =intercalate "\n"
    [ "Welcome " <> waveEmoji <> "!"
    , "I am here to help you to try out some PureScript code!"
    , "I react to messages that start with @royo and are followed by code in in between backticks. "
    , "For example:"
    , ""
    , "@royo"
    , "\\`\\`\\`"
    , "\"Hello World!\""
    , "\\`\\`\\`"
    ]
    
sendRunProblem ∷ Message -> String -> String -> Aff Unit
sendRunProblem msg code output = do
  msg # react sirenEmoji
  dmc <- createDMChannel msg.author
  let say x = sendString (NonEmptyString x) dmc
  say $ "Hey there! I just tried to run your code: \n"
    <> prepareOutput code
    <> "\nHowever, we can still improve a few things about it."
    <> "This is the intel I could gather:"
  say $ output
  say $ "Don't worry, simply edit your original code and I will try again! The compiler and I are here to help you! We're all in this together " <> robotMuscleEmoji

sendCompileProblem ∷ Message -> String -> CompileResult -> Aff Unit
sendCompileProblem msg code cr = do
  msg # react sirenEmoji
  dmc <- createDMChannel msg.author
  let say x = sendString (NonEmptyString x) dmc
  say $ "Hey there! I just tried to compile your code: \n"
    <> prepareOutput code
    <> "\nHowever, we can still improve a few things about it."
    <> "This is what the compiler says:"
  say $ compilerMessages
  say $ "Don't worry, simply edit your original code and I will try again! The compiler and I are here to help you! We're all in this together " <> robotMuscleEmoji
  where
    compilerMessages = intercalate "\n" (prepareOutput <$> (cr.result <#> _.message))

prepareOutput ∷ String -> String
prepareOutput s =
  if contains (Pattern "\n") s then
    "```\n" <> s <> "```\n"
  else
    "`" <> s <> "`"

prepareCode ∷ String -> String
prepareCode code =
  if startsWith "module" code then
    code
  else
    if startsWith "import " code then
      moduleMain <> code
    else
      if (startsWith "main =" || contains (Pattern "\nmain =")) code then
        moduleMain <> importBasics <> code
      else
        moduleMain <> importBasics <> spyMain <> code 
  where
    moduleMain = "module Main where\n"

    importBasics = "import Basics\n"

    spyMain = "main = log $ unsafeCoerce $ "

isMention ∷ String -> Boolean
isMention = startsWith mentionPrefix1 || startsWith mentionPrefix2

parseCodeBlock ∷ String -> Maybe String
parseCodeBlock =
  pure <<< trim
    >=> pure
    <<< drop (length mentionPrefix1)
    >=> (pure <<< trim)
    >=> (lift2 alt (stripPrefix (Pattern "```purescript")) (stripPrefix (Pattern "```")))
    >=> stripSuffix (Pattern "```")
    >=> (pure <<< trim)

rewindEmoji ∷ String
rewindEmoji = "⏪"

robotMuscleEmoji ∷ String
robotMuscleEmoji = "🦾"

sirenEmoji ∷ String
sirenEmoji = "🚨"

thinkingEmoji ∷ String
thinkingEmoji = "🤔"

disappointedEmoji ∷ String
disappointedEmoji = "😔"

waveEmoji ∷ String
waveEmoji = "👋"
