module Events
  ( MyEvent(..)
  , eventProducer
  , inputConsumer
  , setupEventLoop
  )
  where

import Prelude

import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (emit, produce')
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.State (lift)
import Data.Either (Either(..))
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event, EventTarget)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

data MyEvent
    = MouseDown Event
    | MouseMove Event
    | Draw

eventProducer :: forall m. MonadAff m => Int -> EventTarget -> Producer MyEvent m Unit
eventProducer waitTime target = produce' \emitter -> do
    moveListener <- eventListener $ \e -> emit emitter (MouseMove e)
    downListener <- eventListener $ \e -> emit emitter (MouseDown e)
    addEventListener (EventType "mousemove") moveListener false target
    addEventListener (EventType "mousedown") downListener false target

    launchAff_ $ forever do
        _ <- waitForAnimationFrame waitTime
        liftEffect $ emit emitter Draw

inputConsumer :: forall m e. Monad m => (e -> m Unit) -> Consumer e m Unit
inputConsumer f = forever do
    x <- await
    lift $ f x

setupEventLoop :: forall m. MonadRec m => Consumer MyEvent m Unit -> Producer MyEvent m Unit -> m Unit
setupEventLoop consumer producer = runProcess $ consumer `pullFrom` producer

-- TODO: better to use setTimeout or delay here?
waitForAnimationFrame :: Int -> Aff Unit
waitForAnimationFrame waitTime = makeAff \em -> do
    win <- window
    _ <- setTimeout waitTime $ do
        _ <- requestAnimationFrame (em $ Right unit) win
        pure unit
    pure mempty