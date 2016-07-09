module Component where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Elm.Dict as Dict
import Elm.Dict (Dict(..))
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)
import Belgium (nativeToString)
import Window as W
import WindowReact as WR

data State = State
    { counters :: Dict W.Id Int
    , winsys :: W.WinSys
    , cid :: W.Id
    }

instance showState :: Show State where
  show (State s) = nativeToString s

-- Here is the action type associated with our component:

data Action
    = WinAction W.InAction
    | NewWindow
    | Increment W.Id
    | Decrement W.Id
    | Text W.Id String

initialState :: State
initialState =
  let winsys = W.rootWindow 1000.0 1000.0 in
  let createdWindowFactory =
    -- Create a window whose job is to create new windows
    W.createWindow (W.rootId winsys) winsys (
     W.Bounds { 
        top: 0.0
      , left: 0.0
      , width: 160.0
      , height: 120.0
      , title: "Create Window" 
      }
    ) 
  in
  State { 
    counters: Dict.empty
  , winsys: snd createdWindowFactory
  , cid: fst createdWindowFactory
  }

-- The first argument to the render function is a callback which
-- can be used to invoke actions.
--
-- Notice how the action gets attached to event handlers such as 
-- onClick.

render :: T.Render State _ _
render dispatch _ (State state) _ =
  let displayStateCounter c wid =
    [ R.h1' [ R.text "Lesson 2 - Actions" ]
    , R.p' [ 
        R.text "The state is: "
      , R.text (show c)
      ]
    , R.p [ RP.className "btn-group" ] [ 
        R.button [ 
          RP.className "btn btn-success"
        , RP.onClick \_ -> dispatch (Increment wid)
        ]
        [ R.text "Increment" ]
      , R.button 
        [ RP.className "btn btn-danger"
        , RP.onClick \_ -> dispatch (Decrement wid)
        ]
        [ R.text "Decrement" ]
      , R.input 
        [ RP.className "input"
        , RP.onInput \e -> dispatch (Text wid (unsafeCoerce e).target.value)
        , RP.value (nativeToString c)
        ]
        []
      ]
    ]
  in
  let displayWindowCreator _ =
    [ R.h1 [ 
        RP.onClick \_ -> dispatch NewWindow ] [ 
          R.text "Create a new window" 
        ] 
    ]
  in
  let unknownWindow _ = [ ]
  in
  let getWindowDisplayFun wid =
    case Dict.get wid state.counters of
        Just c -> 
          displayStateCounter c wid
        Nothing ->
          if eq state.cid wid then displayWindowCreator unit else unknownWindow unit
  in
  WR.createReactTree (\a -> dispatch (WinAction a)) getWindowDisplayFun state.winsys
  
-- The performAction function is responsible for responding to an action
-- by returning a coroutine which emits state updates.
--
-- A simple coroutine emits a single state update using the 'cotransform'
-- function.
--
-- The coroutine type can also be used asynchronously, as we will see in
-- the next lesson.

applyFunctionToCounter f wid (State state) =
    case Dict.get wid state.counters of
      Just c ->
        (State (state { counters = Dict.insert wid (f c) state.counters }))
      Nothing ->
        (State state)

performAction :: T.PerformAction _ State _ Action
performAction (Increment wid) _ _ update =
  update $ \(State state) ->
    applyFunctionToCounter (\x -> x + 1) wid (State state)
    
performAction (Decrement wid) _ _ update =
  update $ \(State state) ->
    applyFunctionToCounter (\x -> x - 1) wid (State state)    

performAction (Text wid s) _ _ update =
  update $ \(State state) ->
    applyFunctionToCounter (\x ->
      case Int.fromString s of
        Just i -> i
        Nothing -> x
      ) wid (State state)

performAction (WinAction wa) _ _ update = update $ \(State state) ->
  let updatedWinsys = W.update wa state.winsys in
  State (state { winsys = updatedWinsys })

performAction (NewWindow) _ _ update =
  update $ \(State state) ->
  let updatedWinsys =
    W.createWindow (W.rootId state.winsys) state.winsys (
      W.Bounds 
        { left: 20.0
        , top: 300.0
        , width: 350.0
        , height: 170.0
        , title: "Document" 
        }
      ) 
  in
  (( \(Tuple id_ winsys) ->
    State (state { winsys = winsys, counters = Dict.insert id_ 42 state.counters }))
   updatedWinsys)

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
