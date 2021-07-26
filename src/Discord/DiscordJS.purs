module Discord.DiscordJS where

import Prelude
import Control.Promise (Promise, toAff)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prim.Row (class Lacks)

foreign import data Client ∷ # Type → Type

foreign import newClient ∷ Effect (Client ())

foreign import onceReady ∷
  ∀ r.
  Effect Unit →
  Client r →
  Effect Unit

newtype DiscordToken
  = DiscordToken String

foreign import loginImpl ∷
  ∀ r.
  DiscordToken →
  Client ( | r ) →
  Effect (Promise (Client ( loggedIn ∷ Void | r )))

login ∷
  ∀ r.
  Lacks "loggedIn" r ⇒
  DiscordToken →
  Client r →
  Aff (Client ( loggedIn ∷ Void | r ))
login t c = do
  p ← loginImpl t c # liftEffect
  toAff p

type Message
  = { channel ∷ Channel
    , content ∷ String {- not really... -}
    , author ∷ User
    }

foreign import removeAllReactionsImpl ∷ Message → Effect (Promise Unit)

removeAllReactions ∷ Message → Aff Unit
removeAllReactions message = do
  removed ← removeAllReactionsImpl message # liftEffect
  toAff removed

foreign import reactImpl ∷ String → Message → Effect (Promise Unit)

data ChannelType
  = DirectMessage
  | TextChannel
  | OtherChannel String

derive instance eqChannelType ∷ Eq ChannelType

foreign import getChannelTypeImpl ∷ Channel → String

getChannelType ∷ Channel → ChannelType
getChannelType s = case getChannelTypeImpl s of
  "dm" → DirectMessage
  "text" → TextChannel
  other → OtherChannel other

react ∷ String → Message → Aff Unit
react reaction message = do
  p ← reactImpl reaction message # liftEffect
  toAff p

foreign import data Channel ∷ Type

foreign import onMessage ∷
  ∀ r. (Message → Effect Unit) → Client ( loggedIn ∷ Void | r ) → Effect Unit

foreign import onMessageUpdate ∷
  ∀ r. (Message → Message → Effect Unit) → Client ( loggedIn ∷ Void | r ) → Effect Unit

foreign import sendStringImpl ∷ NonEmptyString → Channel → Effect (Promise Unit)

sendString ∷ NonEmptyString → Channel → Aff Unit
sendString s c = do
  (sendStringImpl s c # liftEffect) >>= toAff

foreign import data User ∷ Type

foreign import createDMChannelImpl ∷ User → Effect (Promise Channel)
createDMChannel ∷ User → Aff Channel
createDMChannel u = do
  c ← createDMChannelImpl u # liftEffect
  toAff c
