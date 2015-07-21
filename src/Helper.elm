module Helper where

import Color exposing (Color)
import Html exposing (Html, Attribute)
import Html.Events
import Signal exposing (Address)
import Json.Decode exposing (Decoder, (:=))
import Debug

type alias Vector =
  { x : Float , y : Float }

infixl 2 =>
(=>) = (,)

toRgbaString : Color -> String
toRgbaString color =
  let {red, green, blue, alpha} = Color.toRgb color
  in
      "rgba(" ++ toString red ++ ", " ++ toString green ++ ", " ++ toString blue ++ ", " ++ toString alpha ++ ")"


type alias Update action state = action -> state -> state
type alias View action state = Address action -> state -> Html
type alias Init options state = Vector -> options -> state

localPositionDecoder : Decoder Vector
localPositionDecoder =
  Json.Decode.object2 Vector
    ("clientX" := Json.Decode.float)
    ("clientY" := Json.Decode.float)


type alias TouchEvent =
  { changedTouches : List Touch }

touchEvent : Decoder TouchEvent
touchEvent =
  Json.Decode.object1 TouchEvent
    ("changedTouches" := Json.Decode.list touch)

type alias Touch =
  { pageX : Float
  , pageY : Float
  , clientX : Float
  , clientY : Float
  }

touch : Decoder Touch
touch =
  Json.Decode.object4 Touch
    ("pageX" := Json.Decode.float)
    ("pageY" := Json.Decode.float)
    ("clientX" := Json.Decode.float)
    ("clientY" := Json.Decode.float)


toLocalPosition : TouchEvent -> Result String Vector
toLocalPosition event =
  case event.changedTouches of
    [] ->
      Err "No Touches"

    { pageX, pageY, clientX, clientY } :: _ ->
      Ok
        { x = pageX - clientX
        , y = pageY - clientY
        }


touchDecoder : Decoder Vector
touchDecoder =
  Json.Decode.customDecoder touchEvent toLocalPosition

options =
  { stopPropagation = True
  , preventDefault = True
  }

event : String -> Address a -> (Vector -> a) -> Attribute
event name address constructor =
  Html.Events.onWithOptions name options localPositionDecoder (constructor >> Signal.message address)

onMouseDown =
  event "mousedown"

onMouseMove =
  event "mousemove"

onMouseUp =
  event "mouseup"

onMouseLeave =
  event "mouseleave"

onMouseOut =
  event "mouseout"

onTouchStart : Address a -> (Vector -> a) -> Attribute
onTouchStart =
  event "touchstart"

onTouchEnd : Address a -> (Vector -> a) -> Attribute
onTouchEnd =
  event "touchend"

onTouchMove : Address a -> (Vector -> a) -> Attribute
onTouchMove =
  event "touchmove"

onTouchCancel : Address a -> (Vector -> a) -> Attribute
onTouchCancel =
  event "touchcancel"

onTouchLeave : Address a -> (Vector -> a) -> Attribute
onTouchLeave =
  event "touchLeave"
