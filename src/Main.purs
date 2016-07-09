module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as R
import React (createFactory) as R
import ReactDOM (render) as R
import React.DOM.Props as RP
import Thermite as T
import Component as C

-- | The main method creates the task list component, and renders it to the document body.
main :: forall eff. Eff (dom :: DOM.DOM | eff) Unit
main = void do
  let spec = C.spec
  let initialState = C.initialState
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document)
  R.render (R.createFactory component unit) container
