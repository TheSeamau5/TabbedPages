module Components.Label where

import Html exposing (Html)
import Html.Attributes
import Helper exposing (..)
import Color exposing (Color)

type alias State =
  { value : String
  , size  : Vector
  , color : Color
  }

type alias Options =
  { value : String }

init : Init Options State
init size options =
  { value = options.value
  , size  = size
  , color = Color.blue
  }


type Action
  = SetValue String
  | Resize Vector
  | SetColor Color


update : Update Action State
update action state =
  case action of
    SetValue value ->
      { state | value <- value }

    Resize size ->
      { state | size <- size }

    SetColor color ->
      { state | color <- color }



view : View Action State
view address state =
  let
      fontSize =
        sqrt ( 5 * (min state.size.y state.size.x))

      labelStyle =
          [ "position"        => "absolute"
          , "width"           => toString state.size.x ++ "px"
          , "height"          => toString state.size.y ++ "px"
          , "text-align"      => "center"
          , "display"         => "flex"
          , "justify-content" => "center"
          , "align-items"     => "center"
          , "overflow"        => "hidden"
          , "font-size"       => toString fontSize ++ "px"
          , "color"           => toRgbaString state.color
          ]
  in
      Html.div
          [ Html.Attributes.style labelStyle ]
          [ Html.text state.value ]
