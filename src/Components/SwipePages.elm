module Components.SwipePages where

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Helper exposing (..)
import SelectionList exposing (SelectionList)
import Spring exposing (Spring)

type alias State pageState =
  { pages : SelectionList pageState
  , size  : Vector
  , offset : Spring Float
  , threshold : Float
  , isPressed : Bool
  , lastPressed : Float
  }

type alias Options pageOptions =
  { first : pageOptions
  , rest  : List pageOptions
  }

init : Update Context pageState -> Init pageOptions pageState -> Init (Options pageOptions) (State pageState)
init applyContext pageOptions size options =
  let
      pages =
        SelectionList.fromList (pageOptions size options.first) (List.map (pageOptions size) options.rest)

      getContext index =
        { size = size
        , isSelected = index == SelectionList.selectedIndex pages
        }

      pages' =
        pages
        |> SelectionList.indexedMap (getContext >> applyContext)
  in
      { pages = pages'
      , size  = size
      , offset = Spring.create 170 20
      , threshold = 0.2
      , isPressed = False
      , lastPressed = 0
      }

type Action pageAction
  = PageAction Int pageAction
  | Resize Vector
  | Select Int
  | Next
  | Previous
  | SetOffset Float
  | NextFrame Float
  | SetIsPressed Bool
  | SetLastPressed Float
  | NoOp
  | Multiple (List (Action pageAction))


type DragAction
  = Press Float
  | Move Float
  | Release Float

type alias Context =
  { size        : Vector
  , isSelected  : Bool
  }

getContext : Int -> State pageState -> Context
getContext index state =
  { size = state.size
  , isSelected = index == SelectionList.selectedIndex state.pages
  }

update : Update Context pageState -> Update pageAction pageState -> Update (Action pageAction) (State pageState)
update applyContext updateItem action state =
  case action of
    PageAction index pageAction ->
      let
          state' =
            { state | pages <- SelectionList.updateN index (updateItem pageAction) state.pages }

          pages' =
            state'.pages
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

      in
          { state' | pages <- pages' }


    Resize size ->
      let
          state' =
            { state | size <- size }

          pages' =
            state'.pages
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

      in
          { state' |  pages <- pages' }
          |> update applyContext updateItem (Select (SelectionList.selectedIndex state.pages))


    Select index ->
      let
          state' =
            { state | pages <- SelectionList.goto index state.pages }

          pages' =
            state'.pages
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          oldIndex =
            SelectionList.selectedIndex pages'

          offset =
            toFloat oldIndex * state.size.x

      in
          { state' | pages <- pages' }
          |> update applyContext updateItem (SetOffset offset)


    Next ->
      let
          state' =
            { state | pages <- SelectionList.next state.pages }

          pages' =
            state'.pages
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          index =
            SelectionList.selectedIndex pages'

          offset =
            toFloat index * state.size.x

      in
          { state' | pages <- pages' }
          |> update applyContext updateItem (SetOffset offset)


    Previous ->
      let
          state' =
            { state | pages <- SelectionList.previous state.pages }

          pages' =
            state'.pages
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          index =
            SelectionList.selectedIndex pages'

          offset =
            toFloat index * state.size.x

      in
          { state' | pages <- pages' }
          |> update applyContext updateItem (SetOffset offset)

    SetOffset offset ->
      { state | offset <- Spring.setDestination offset state.offset }

    NextFrame frame ->
      { state | offset <- Spring.animate frame state.offset }

    SetIsPressed isPressed ->
      { state | isPressed <- isPressed }

    SetLastPressed lastPressed ->
      { state | lastPressed <- lastPressed }

    Multiple actions ->
      case actions of
        [] ->
          state
        x :: xs ->
          state
          |> update applyContext updateItem x
          |> update applyContext updateItem (Multiple xs)

    NoOp ->
      state



fromDragAction : State pageState -> DragAction -> Action pageAction
fromDragAction state action =
  case action of
    Press x ->
      Multiple
        [ SetIsPressed True
        , SetLastPressed x
        ]

    Move x ->
      if state.isPressed
      then
        let
            delta =
              state.lastPressed - x

            index =
              SelectionList.selectedIndex state.pages

            offset =
              toFloat index * state.size.x + delta

        in
            SetOffset offset
      else
        NoOp

    Release x ->
      if state.isPressed
      then
        let
            change =
              (state.lastPressed - x) / state.size.x

        in
            if | change > state.threshold ->
                  Multiple
                    [ SetIsPressed False
                    , Next
                    ]

               | change < -state.threshold ->
                  Multiple
                    [ SetIsPressed False
                    , Previous
                    ]

               | otherwise ->
                  Multiple
                    [ SetIsPressed False
                    , Select (SelectionList.selectedIndex state.pages)
                    ]

      else
        NoOp



view : View pageAction pageState -> View (Action pageAction) (State pageState)
view viewPage address state =
  let
      containerStyle =
          [ "position" => "absolute"
          , "width" => toString state.size.x ++ "px"
          , "height" => toString state.size.y ++ "px"
          ]

      selectedIndex =
        SelectionList.selectedIndex state.pages

      viewN index page =
        let
            pageAddress =
                Signal.forwardTo address (PageAction index)

            left =
                state.size.x * (toFloat index) - Spring.current state.offset

            pageContainerStyle =
                [ "position"  => "absolute"
                , "width"     => toString state.size.x ++ "px"
                , "height"    => toString state.size.y ++ "px"
                , "transform" => "translate3d(" ++ toString left ++ "px, 0px, 0px)"
                ]
        in
            Html.div
                [ Html.Attributes.style pageContainerStyle ]
                [ viewPage pageAddress page ]

  in
      Html.div
          [ Html.Attributes.style containerStyle
          , Html.Attributes.id "Pages"
          , onMouseDown address (.x >> Press >> fromDragAction state)
          , onMouseMove address (.x >> Move >> fromDragAction state)
          , onMouseUp address (.x >> Release >> fromDragAction state)
          , onMouseLeave address (.x >> Release >> fromDragAction state)
          , onMouseOut address (.x >> Release >> fromDragAction state)
          --, onTouchStart address (.x >> Press)
          --, onTouchMove address (.x >> Move)
          --, onTouchEnd address (.x >> Release)
          --, onTouchLeave address (.x >> Release)
          --, onTouchCancel address (.x >> Release)
          ]
          ( SelectionList.indexedMap viewN state.pages |> SelectionList.toList )
