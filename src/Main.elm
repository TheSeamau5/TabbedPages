import Components.TabbedPages as TabbedPages
import Components.Tabs as Tabs
import Components.SwipePages as Pages
import Components.Label as Label
import Color as Color
import Helper exposing (..)
import Signal exposing (Signal, Address, Mailbox)
import Task exposing (Task)
import Window
import Html exposing (Html)
import AnimationFrame

type alias Application =
  { state : Maybe (TabbedPages.State Label.State Label.State) }


init : Vector -> Application
init size =
  let
      tabHeight = 50

      first =
        ( { value = "Page 0" }
        , { value = "Welcome to the TabbedPages Component" }
        )

      rest =
        [ ( { value = "Page 1" }
          , { value = "This is Page 1"}
          )
        , ( { value = "Page 2" }
          , { value = "And this is Page 2"}
          )
        , ( { value = "Page 3" }
          , { value = "Finally this is the fourth page or Page 3... cuz counting is too hard"}
          )
        ]

      options =
        { tabHeight = tabHeight
        , first = first
        , rest = rest
        }

      state =
        TabbedPages.init applyTabsContext Label.init applyPagesContext Label.init size options
  in
      { state = Just state }


initial : Application
initial =
  { state = Nothing }

type Action
  = Resize Vector
  | ApplicationAction (TabbedPages.Action Label.Action Label.Action)
  | NextFrame Float
  | NoOp


applyTabsContext : Update Tabs.Context Label.State
applyTabsContext context state =
  if context.isSelected
  then
    state
    |> Label.update (Label.SetColor Color.red)
    |> Label.update (Label.Resize context.size)
  else
    state
    |> Label.update (Label.SetColor Color.blue)
    |> Label.update (Label.Resize context.size)


applyPagesContext : Update Pages.Context Label.State
applyPagesContext context state =
  state
  |> Label.update (Label.Resize context.size)


updateState : Update (TabbedPages.Action Label.Action Label.Action) (TabbedPages.State Label.State Label.State)
updateState =
  TabbedPages.update applyTabsContext Label.update applyPagesContext Label.update


update : Update Action Application
update action application =
  case action of
    Resize size ->
      case application.state of
        Nothing ->
          init size

        Just state ->
          { application | state <- Just (updateState (TabbedPages.Resize size) state) }


    ApplicationAction applicationAction ->
      case application.state of
        Nothing ->
          application

        Just state ->
          { application | state <- Just (updateState applicationAction state) }


    NextFrame frame ->
      case application.state of
        Nothing ->
          application

        Just state ->
          { application | state <- Just (updateState (TabbedPages.NextFrame frame) state) }

    NoOp ->
      application


view : View Action Application
view address application =
  case application.state of
    Nothing ->
      Html.text "Loading..."

    Just state ->
      TabbedPages.view Label.view Label.view (Signal.forwardTo address ApplicationAction) state

---------------

appStartMailbox : Mailbox ()
appStartMailbox =
  Signal.mailbox ()


port appStart : Signal (Task error ())
port appStart =
  Signal.send appStartMailbox.address ()
  |> Signal.constant


appMailbox : Mailbox Action
appMailbox =
  Signal.mailbox NoOp

address : Address Action
address =
  appMailbox.address

frames : Signal Action
frames =
  Signal.map NextFrame AnimationFrame.frame

resizes : Signal Action
resizes =
  let
      resize =
        Signal.map (\(x,y) -> Resize { x = toFloat x , y = toFloat y }) Window.dimensions

      firstResize =
        Signal.sampleOn appStartMailbox.signal resize

  in
      Signal.merge firstResize resize


actions : Signal Action
actions =
  Signal.mergeMany
    [ appMailbox.signal
    , resizes
    , frames
    ]


------------------

main =
  Signal.map (view address)
    (Signal.foldp update initial actions)
