module Window
  ( WinSys()
  , Window()
  , rootWindow
  , Bounds(Bounds)
  , Id()
  , Pt(Pt)
  , InAction(..)
  , createWindow
  , destroyWindow
  , rootId
  , update
  , rootWin
  , wid
  , children
  , title
  , bounds
  ) where

import Prelude
import Debug (log)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array as Array
import Data.Array ((!!))
import Belgium (nativeToString)

-- | A window which may contain other windows.
-- | id is an Int for use in an Id
-- | fixed indicates the root window
data Window = Window
    { id :: Int
    , left :: Number
    , top :: Number
    , width :: Number
    , height :: Number
    , title :: String
    , children :: Array Window
    , fixed :: Boolean }

-- | A Show instance for Window for diagnostics.
instance showWindow :: Show Window where
  show (Window s) = nativeToString s

-- | A type indicating what is being dragged.  Each is labeled
-- | with the Id of a window so that we can track the drag without
-- | necessarily having the cursor inside the window at all times.
data DragType
    = NoDrag
    | DragMove Id Pt
    | DragResize Id Pt

-- | The window system.
data WinSys = WinSys
    { root :: Window
    , id :: Int
    , dragStart :: DragType }

-- | A Show instance for a WinSys for Diagnostics.
instance showWinSys :: Show WinSys where
  show (WinSys s) = nativeToString s

-- | An Eq instance for Id, allowing us to test ids for equality.
instance idEq :: Eq Id where
  eq (Id a) (Id b) = a == b

instance idOrd :: Ord Id where
  compare (Id a) (Id b) = compare a b

-- | A constructor for the window system, indicating the root window gemoetry.
rootWindow :: Number -> Number -> WinSys
rootWindow w h =
    WinSys 
        { root: Window 
          { id: 0
          , left: 0.0
          , top: 0.0
          , width: w
          , height: h
          , title: ""
          , children: [] 
          , fixed: true }
        , id: 1
        , dragStart: NoDrag
        }

-- | A description of a window to create.
data Bounds = Bounds
    { left :: Number
    , top :: Number
    , width :: Number
    , height :: Number
    , title :: String
    }

-- | the Id of a window.
data Id = Id Int

-- | A point
data Pt
    = Pt { x :: Number, y :: Number }

-- | The input type of this component.  We'll be responding to events of these types.
-- | I wasn't going to originally, but realized that following the usual nesting
-- | pattern for components in elm and systems like it was appropriate here despite
-- | that it initially seemed cumbersome.
data InAction
    = MouseDown Pt
    | MouseMove Pt
    | MouseUp

{-
-- Internal method to create a window in the given parent's child set given a bounds
-- to describe it.
-}
insertWindowChild (Id id) (Id p) (Window win) (Bounds bounds) =
    if p == win.id then
      Window (
        win {
          children =
            Array.cons (
              Window {
                  id: id
                , left: bounds.left
                , top: bounds.top
                , width: bounds.width
                , height: bounds.height
                , title: bounds.title
                , children: [], fixed: false
                }
              ) win.children
        }
      )
    else
      Window (
        win {
            children =
              Array.concatMap
                (\w ->
                  Array.singleton (
                    insertWindowChild (Id id) (Id p) w (Bounds bounds)
                  )
                ) win.children
          }
      )

{-
-- Given a predicate and a window, apply the update function as required by the predicate
-- recursively
-}
applyUpdateWindowInSystem pred update (Window win) =
  let applyUpdateToChild (Window w) =
    Array.singleton (applyUpdateWindowInSystem pred update (Window w))
  in
  let childUpdates =
    Array.concatMap applyUpdateToChild win.children
  in
  let childUpdateToCounterList (Tuple updated win) =
    if updated then Array.singleton unit else [] 
  in
  let childGotUpdate = 
    Array.length (Array.concatMap childUpdateToCounterList childUpdates) /= 0
  in
  if childGotUpdate then
      Tuple true (
        Window (
          win { 
              children = 
                Array.concatMap 
                  ( \(Tuple updated win) -> Array.singleton win ) childUpdates
            }
        )
      )
  else if pred (Window win) then
    Tuple true (update (Window win))
  else
    Tuple false (Window win)

{-
-- Generic point inside window
-}
windowPtInside (Pt pt) (Window w) =
  w.left <= pt.x &&
  w.top <= pt.y &&
  (w.left + w.width) > pt.x &&
  (w.top + w.height) > pt.y

{-
-- Point in resize handle
-}
windowResizePt (Pt pt) (Window w) =
  (w.left + w.width) > pt.x &&
  (w.top + w.height) > pt.y &&
  (w.left + w.width - 30.0) <= pt.x &&
  (w.top + w.height - 30.0) <= pt.y

{-
-- Point in draggable title
-}
windowMovePt (Pt pt) (Window w) =
  w.left <= (pt.x + 10.0) &&
  (w.left + w.width) > pt.x - 10.0 &&
  w.top <= (pt.y + 10.0) &&
  (w.top + 30.0) > pt.y

{-
-- This is one of those rare times that the pattern so admired by textbooks, composing a
-- single binary operator into a function, actually gives you something better than what
-- you started with.
-}
moveableAnd f (Pt pt) (Window w) =
  (not w.fixed) && (f (Pt pt) (Window w))

{-
-- Return a list of DragType in priority order for matched entities.
-- This model gives us some good properties:
-- * We could later stable sort by another priority (such as drag first or resize first, or even an arbitrary priority)
-- * We don't need special code to handle conflicts between siblings or parent and child
-- If this window doesn't contain the point then bail early avoiding all children.
-}
determineDragType (Pt pt) (Window w) =
  let childUpdate =
    Array.concatMap ( \(Window w) -> determineDragType (Pt pt) (Window w) ) w.children 
  in
  let dragResizeResult _ = 
    [ DragResize (Id w.id) (Pt pt) ]
  in
  let dragResizeCorner =
    if (moveableAnd windowResizePt) (Pt pt) (Window w) then dragResizeResult unit else [] 
  in
  let dragMoveResult _ =
    [ DragMove (Id w.id) (Pt pt) ]
  in
  let dragMoveWindow = 
    if (moveableAnd windowMovePt) (Pt pt) (Window w) then dragMoveResult unit else [] 
  in
  let getDragType _ =
    case (Array.concat [ childUpdate, dragResizeCorner, dragMoveWindow ]) !! 0 of
        Just x -> [ x ]
        Nothing -> []
  in
    if not windowPtInside (Pt pt) (Window w) then [] else getDragType unit

windowHasId (Id id) (Window w) =
  eq (Id id) (Id w.id)

{-
-- Mouse down needs to start a drag.  A successful DragType will contain a window id
-- which will apply to mouse move.  This sidesteps the problem that a mouse drag can
-- be quick and discontinuous and leave behind the window it's dragging unless there's
-- some kind of capture.
-}
applyWindowUpdate (MouseDown (Pt pt)) (WinSys winsys) =
  let dragType = 
    case (determineDragType (Pt pt) winsys.root) !! 0 of
        Just x -> x
        Nothing -> NoDrag
  in
  WinSys (winsys { dragStart = dragType })
{-
-- Track mouse movement.  If a mouse down event preceeded this one and determineDragType
-- return a result, then we'll apply the mouse movement to the window whose id was captured
-- by drag type.  Happily, if the window is deleted, then continuing to drag is benign.
-}
applyWindowUpdate (MouseMove (Pt pt)) (WinSys winsys) =
  case winsys.dragStart of
      DragMove (Id id) (Pt start) ->
        let winDragUpdate = \(Window w) ->
          Window (w { left = pt.x - start.x + w.left, top = pt.y - start.y + w.top })
        in
        let updatedWin =
          applyUpdateWindowInSystem (windowHasId (Id id)) winDragUpdate winsys.root
        in
        WinSys (winsys { root = snd updatedWin, dragStart = DragMove (Id id) (Pt pt) })
      DragResize (Id id) (Pt start) ->
        let winDragUpdate = \(Window w) ->
          Window (w { width = pt.x - start.x + w.width, height = pt.y - start.y + w.height })
        in
        let updatedWin =
          applyUpdateWindowInSystem (windowHasId (Id id)) winDragUpdate winsys.root
        in
        WinSys (winsys { root = snd updatedWin, dragStart = DragResize (Id id) (Pt pt) })
      _ -> WinSys winsys

{-
-- Unset the drag.
-}
applyWindowUpdate MouseUp (WinSys winsys) =
    WinSys (winsys { dragStart = NoDrag })

-- | Create a window as a child of another window's ID.  Initially, the only Id the user has
-- | access to is that of the root window, available by rootId below.
-- | Returns a tuple of Id and updated window system.
createWindow (Id p) (WinSys winsys) (Bounds bounds) =
    let ws = (winsys { id = winsys.id + 1 }) in
    Tuple (Id winsys.id) (
      WinSys (
        ws {
            root = insertWindowChild (Id winsys.id) (Id p) ws.root (Bounds bounds)
          }
      )
    )

{-
-- Internal method which removes any window with the given ID from a child list.
-}
removeWindowChild (Id id_) (Window w) =
  let removeMatchingChildWindow (Window cw) =
    if cw.id == id_ then [] else Array.singleton (removeWindowChild (Id id_) (Window cw))
  in
  let children =
    Array.concatMap removeMatchingChildWindow w.children 
  in
  Window (w { children = children })

-- | Remove a window by Id from the window system.
destroyWindow (Id id_) (WinSys winsys) =
  WinSys (winsys { root = removeWindowChild (Id id_) winsys.root })

-- | Return the Id of the root window
rootId (WinSys winsys) =
  (( \(Window w) -> (Id w.id)) winsys.root)

-- | Apply an InAction to the WinSys yielding a WinSys.
update = applyWindowUpdate

-- | Get the root window
rootWin (WinSys winsys) = winsys.root

-- | Get the children of a window
children (Window w) = w.children

-- | Get the Id of a window
wid (Window w) = (Id w.id)

-- | Get the title of a window
title (Window w) = w.title

-- | Get the bounds of a window
bounds (Window w) =
  Bounds
    { top: w.top
    , left: w.left
    , width: w.width
    , height: w.height
    , title: w.title
    }
