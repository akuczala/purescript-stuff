module Events
  ( MyEvent(..)
  , eventProducer
  , inputConsumer
  , setupEventLoop
  ) where

import Prelude

import Control.Coroutine (Consumer, Producer, await, pullFrom, runProcess)
import Control.Coroutine.Aff (Emitter, emit, produce')
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.State (lift)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event, EventTarget)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame, toEventTarget)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)
import Web.UIEvent.WheelEvent.EventTypes (wheel)

data MyEvent
  = MouseDown Event
  | MouseUp Event
  | MouseMove Event
  | KeyDown Event
  | Wheel Event
  | Draw

makeAddListener
  :: forall e r
   . Emitter Effect e r
  -> EventTarget
  -> EventType
  -> (Event -> e)
  -> Effect Unit
makeAddListener emitter target eventType mkEvent = do
  listener <- eventListener $ \e -> emit emitter (mkEvent e)
  addEventListener eventType listener false target

eventProducer
  :: forall m
   . MonadAff m
  => Int
  -> EventTarget
  -> Producer MyEvent m Unit
eventProducer waitTime target = produce' \emitter -> do
  traverse_ ((#) emitter)
    [ makeAddMouseListeners
    , makeAddKeyboardListeners
    , makeAddDrawListener
    ]

  where
  makeAddMouseListeners emitter = do
    traverse_ (uncurry $ makeAddListener emitter target)
      [ Tuple mousedown MouseDown
      , Tuple mousemove MouseMove
      , Tuple mouseup MouseUp
      , Tuple wheel Wheel
      ]

  makeAddKeyboardListeners emitter = do
    windowTarget <- map toEventTarget window
    makeAddListener emitter windowTarget keydown KeyDown

  makeAddDrawListener emitter = launchAff_ $ forever do
    _ <- waitForAnimationFrame waitTime
    liftEffect $ emit emitter Draw

inputConsumer :: forall m e. Monad m => (e -> m Unit) -> Consumer e m Unit
inputConsumer f = forever do
  x <- await
  lift $ f x

setupEventLoop
  :: forall m
   . MonadRec m
  => Consumer MyEvent m Unit
  -> Producer MyEvent m Unit
  -> m Unit
setupEventLoop consumer producer = runProcess $ consumer `pullFrom` producer

-- TODO: better to use setTimeout or delay here?
waitForAnimationFrame :: Int -> Aff Unit
waitForAnimationFrame waitTime = makeAff \em -> do
  win <- window
  _ <- setTimeout waitTime $ do
    _ <- requestAnimationFrame (em $ Right unit) win
    pure unit
  pure mempty