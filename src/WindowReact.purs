module WindowReact where

import Prelude

import Debug (log)
import Data.Array as Array
import Data.String as String
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)
import Belgium (nativeToString)
import Window as W

pxValue n =
    String.joinWith "" [ nativeToString n, "px" ]

createReactTreeWindow winsys dispatch childContentFun win =
    let windowChildren = 
      Array.concat [ 
          [ R.h1
            [ RP.className "window-title" ]
            [ R.text (W.title win) ]
          , R.h1
            [ RP.className "window-resize" ]
            [ ]
          , R.div [ RP.className "window-body" ] (childContentFun (W.wid win))
          ]
        , Array.concatMap (createReactTreeWindow winsys dispatch childContentFun) (W.children win)
        ]
    in
    let mouseCover = 
           [ RP.onMouseDown \e -> dispatch (W.MouseDown (W.Pt { x: e.pageX, y: e.pageY }))
           , RP.onMouseMove \e -> dispatch (W.MouseMove (W.Pt { x: e.pageX, y: e.pageY }))
           , RP.onMouseUp \_ -> dispatch W.MouseUp
           ]
    in
    let bound = ((\(W.Bounds b) -> b) (W.bounds win)) in
    [ R.div
      (Array.concat [
        [ RP.className "window-frame"
        , RP.style
          { top: pxValue bound.top
          , left: pxValue bound.left
          , width: pxValue bound.width
          , height: pxValue bound.height
          }
        ]
      , if W.wid win `eq` W.rootId winsys then mouseCover else []
      ]) windowChildren
    ]

createReactTree dispatch childContentFun winsys =
    createReactTreeWindow winsys dispatch childContentFun (W.rootWin winsys)
