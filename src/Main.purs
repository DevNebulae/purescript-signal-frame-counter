module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (setTextContent)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Prelude (Unit, bind, pure, show, unit, ($), (+), (-), (/), (<>))
import Signal (Signal, foldp, runSignal, (~>))
import Signal.DOM (animationFrame)
import Signal.Time (Time)
import Util (selectElement)

data Msg
	= Tick Time

type State =
  { frame :: Int
	, fps :: Maybe Number
	, lastFrame :: Maybe Time
  }

initialState :: State
initialState =
  { frame: 0
	, fps: Nothing
	, lastFrame: Nothing
  }

update :: Msg -> State -> State
update msg state =
	case msg of
		Tick n -> state
			{ frame = state.frame + 1
			, fps = maybe Nothing (\x -> Just $ 1000.0 / (n - x)) state.lastFrame
			, lastFrame = Just n
			}

animationFrameToMsg :: Number -> Msg
animationFrameToMsg = \n -> Tick n

application :: forall eff. Eff (dom :: DOM, timer :: TIMER | eff) (Signal State)
application = do
	frame <- animationFrame
	pure $ foldp update initialState (frame ~> animationFrameToMsg)

logState :: forall eff. State -> Eff (dom :: DOM | eff) Unit
logState state = do
	maybeElement <- selectElement $ QuerySelector "#fps"

	case maybeElement of
		Nothing -> pure unit

		Just e -> setTextContent (maybe "FPS: unknown" (\fps -> "FPS: " <> (show $ round fps)) state.fps) $ htmlElementToNode e

main :: forall eff. Eff (dom :: DOM, timer :: TIMER | eff) Unit
main = do
	app <- application
	runSignal $ app ~> logState
