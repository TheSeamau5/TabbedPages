module Components.Tabs where

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Helper exposing (..)
import SelectionList exposing (SelectionList)
import Spring exposing (Spring)
import Color exposing (Color)

type alias State itemState =
  { items : SelectionList itemState
  , size  : Vector
  , offset : Spring Float
  , stripColor : Color
  }


type alias Options itemOptions =
  { first : itemOptions
  , rest  : List itemOptions
  }

init : Update Context itemState -> Init itemOptions itemState -> Init (Options itemOptions) (State itemState)
init applyContext initItem size options =
  let
      itemSize =
          { x = size.x / (toFloat (List.length options.rest + 1))
          , y = size.y
          }

      items =
        SelectionList.fromList (initItem itemSize options.first) (List.map (initItem itemSize) options.rest)

      getContext index =
        { size = itemSize
        , isSelected = index == SelectionList.selectedIndex items
        }

      items' =
        items
        |> SelectionList.indexedMap (getContext >> applyContext)

  in
      { items = items'
      , size  = size
      , offset = Spring.create 170 20
      , stripColor = Color.red
      }


type Action itemAction
  = ItemAction Int itemAction
  | Resize Vector
  | Select Int
  | Next
  | Previous
  | SetOffset Float
  | NextFrame Float

type alias Context =
  { size : Vector
  , isSelected : Bool
  }

getItemSize : State itemState -> Vector
getItemSize state =
  { x = state.size.x / (toFloat (SelectionList.length state.items))
  , y = state.size.y
  }

getContext : Int -> State itemState -> Context
getContext index state =
  { size = getItemSize state
  , isSelected = index == SelectionList.selectedIndex state.items
  }

update : Update Context itemState -> Update itemAction itemState -> Update (Action itemAction) (State itemState)
update applyContext updateItem action state =
  case action of
    ItemAction index itemAction ->
      let
          state' =
            { state | items <- SelectionList.updateN index (updateItem itemAction) state.items }

          items' =
            state'.items
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

      in
          { state' | items <- items' }

    Resize size ->
      let
          state' =
            { state | size <- size }

          items' =
            state'.items
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))
      in
          { state' |  items <- items' }
          |> update applyContext updateItem (Select (SelectionList.selectedIndex state.items))


    Select index ->
      let
          state' =
            { state | items <- SelectionList.goto index state.items }

          items' =
            state'.items
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          itemSize =
            getItemSize state

          offset =
            toFloat index * itemSize.x

      in
          { state' | items <- items' }
          |> update applyContext updateItem (SetOffset offset)

    Next ->
      let
          state' =
            { state | items <- SelectionList.next state.items }

          items' =
            state'.items
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          itemSize =
            getItemSize state

          index =
            SelectionList.selectedIndex items'

          offset =
            toFloat index * itemSize.x

      in
          { state' | items <- items' }
          |> update applyContext updateItem (SetOffset offset)


    Previous ->
      let
          state' =
            { state | items <- SelectionList.previous state.items }

          items' =
            state'.items
            |> SelectionList.indexedMap (\index -> applyContext (getContext index state'))

          itemSize =
            getItemSize state

          index =
            SelectionList.selectedIndex items'

          offset =
            toFloat index * itemSize.x

      in
          { state' | items <- items' }
          |> update applyContext updateItem (SetOffset offset)

    SetOffset offset ->
      { state | offset <- Spring.setDestination offset state.offset }

    NextFrame frame ->
      { state | offset <- Spring.animate frame state.offset }

view : View itemAction itemState -> View (Action itemAction) (State itemState)
view viewItem address state =
  let
      containerStyle =
          [ "position"  => "absolute"
          , "width"     => toString state.size.x ++ "px"
          , "height"    => toString state.size.y ++ "px"
          , "overflow"  => "hidden"
          ]

      itemSize =
        getItemSize state

      stripLeft =
        Spring.current state.offset

      stripHeight =
        3

      stripTop =
        itemSize.y - stripHeight

      stripStyle =
          [ "position"  => "absolute"
          , "width"     => toString itemSize.x ++ "px"
          , "height"    => toString stripHeight ++ "px"
          , "transform" => "translate3d(" ++ toString stripLeft ++ "px, " ++ toString stripTop ++ "px, 0px)"
          , "z-index"   => "1"
          , "background-color" => toRgbaString state.stripColor
          ]

      strip =
        Html.div
            [ Html.Attributes.style stripStyle ]
            [ ]

      viewN index item =
        let
            itemAddress =
              Signal.forwardTo address (ItemAction index)

            left =
              itemSize.x * (toFloat index)

            itemContainerStyle =
              [ "position"  => "absolute"
              , "width"     => toString itemSize.x ++ "px"
              , "height"    => toString itemSize.y ++ "px"
              , "transform" => "translate3d(" ++ toString left ++ "px, 0px, 0px)"
              , "cursor"    => "pointer"
              ]

        in
            Html.div
                [ Html.Attributes.style itemContainerStyle
                , Html.Events.onClick address (Select index)
                ]
                [ viewItem itemAddress item ]

  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          ( strip :: SelectionList.toList (SelectionList.indexedMap viewN state.items) )
