module Util
	( selectElement
	) where

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector, querySelector)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Prelude (bind, const, pure, ($), (<<<), (<=<), (=<<))

selectElement :: forall eff. QuerySelector -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
selectElement query = do
	maybeElement <- (querySelector query <<< htmlDocumentToParentNode <=< document) =<< window

	pure case maybeElement of
		Nothing -> Nothing
		Just element -> either (const Nothing) Just $ runExcept $ readHTMLElement $ toForeign element
