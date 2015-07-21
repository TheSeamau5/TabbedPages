module Components.TabbedPages where

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Helper exposing (..)
import SelectionList exposing (SelectionList)
import Color exposing (Color)

import Components.Tabs as Tabs
import Components.SwipePages as Pages

type alias State tabState pageState =
  { tabs  : Tabs.State tabState
  , pages : Pages.State pageState
  , size  : Vector
  , tabHeight : Float
  }


type alias Options tabOptions pageOptions =
  { tabHeight : Float
  , first : (tabOptions, pageOptions)
  , rest  : List (tabOptions, pageOptions)
  }


init : Update Tabs.Context tabState
    -> Init tabOptions tabState
    -> Update Pages.Context pageState
    -> Init pageOptions pageState
    -> Init (Options tabOptions pageOptions) (State tabState pageState)
init applyTabContext initTab applyPageContext initPage size options =
  let
      tabsSize =
        { x = size.x
        , y = options.tabHeight
        }

      pagesSize =
        { x = size.x
        , y = size.y - options.tabHeight
        }

      (restTabs, restPages) =
        List.unzip options.rest

      (firstTabs, firstPages) =
        options.first

      tabsOptions =
        { first = firstTabs
        , rest  = restTabs
        }

      pagesOptions =
        { first = firstPages
        , rest  = restPages
        }

  in
      { tabs = Tabs.init applyTabContext initTab tabsSize tabsOptions
      , pages = Pages.init applyPageContext initPage pagesSize pagesOptions
      , size = size
      , tabHeight = options.tabHeight
      }


type Action tabAction pageAction
  = TabsAction (Tabs.Action tabAction)
  | PagesAction (Pages.Action pageAction)
  | Resize Vector
  | NextFrame Float


getTabsSize : State tabState pageState -> Vector
getTabsSize state =
  { x = state.size.x
  , y = state.tabHeight
  }


getPagesSize : State tabState pageState -> Vector
getPagesSize state =
  { x = state.size.x
  , y = state.size.y - state.tabHeight
  }


update : Update Tabs.Context tabState
      -> Update tabAction tabState
      -> Update Pages.Context pageState
      -> Update pageAction pageState
      -> Update (Action tabAction pageAction) (State tabState pageState)
update applyTabContext updateTab applyPageContext updatePage action state =
  case action of
    TabsAction (Tabs.Select index) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.Select index) state.tabs
              , pages <- Pages.update applyPageContext updatePage (Pages.Select index) state.pages
      }

    TabsAction (Tabs.Next) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab Tabs.Next state.tabs
              , pages <- Pages.update applyPageContext updatePage Pages.Next state.pages
      }

    TabsAction (Tabs.Previous) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab Tabs.Previous state.tabs
              , pages <- Pages.update applyPageContext updatePage Pages.Previous state.pages
      }

    TabsAction (Tabs.NextFrame frame) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.NextFrame frame) state.tabs
              , pages <- Pages.update applyPageContext updatePage (Pages.NextFrame frame) state.pages
      }

    TabsAction (Tabs.SetOffset offset) ->
      let
          pagesOffset =
            toFloat (SelectionList.length state.pages.pages) * offset
      in
          { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.SetOffset offset) state.tabs
                  , pages <- Pages.update applyPageContext updatePage (Pages.SetOffset pagesOffset) state.pages
          }

    TabsAction tabsAction ->
      { state | tabs <- Tabs.update applyTabContext updateTab tabsAction state.tabs }

    PagesAction (Pages.Select index) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.Select index) state.tabs
              , pages <- Pages.update applyPageContext updatePage (Pages.Select index) state.pages
      }

    PagesAction (Pages.Next) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab Tabs.Next state.tabs
              , pages <- Pages.update applyPageContext updatePage Pages.Next state.pages
      }

    PagesAction (Pages.Previous) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab Tabs.Previous state.tabs
              , pages <- Pages.update applyPageContext updatePage Pages.Previous state.pages
      }

    PagesAction (Pages.NextFrame frame) ->
      { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.NextFrame frame) state.tabs
              , pages <- Pages.update applyPageContext updatePage (Pages.NextFrame frame) state.pages
      }


    PagesAction (Pages.SetOffset offset) ->
      let
          tabsOffset =
            offset / toFloat (SelectionList.length state.tabs.items)
      in
          { state | tabs  <- Tabs.update applyTabContext updateTab (Tabs.SetOffset tabsOffset) state.tabs
                  , pages <- Pages.update applyPageContext updatePage (Pages.SetOffset offset) state.pages
          }

    PagesAction (Pages.Multiple actions) ->
      case actions of
        [] ->
          state

        x :: xs ->
          state
          |> update applyTabContext updateTab applyPageContext updatePage (PagesAction x)
          |> update applyTabContext updateTab applyPageContext updatePage (PagesAction (Pages.Multiple xs))

    PagesAction pagesAction ->
      { state | pages <- Pages.update applyPageContext updatePage pagesAction state.pages }

    Resize size ->
      let
          state' =
            { state | size <- size }

      in
          { state' | tabs  <- Tabs.update applyTabContext updateTab (Tabs.Resize (getTabsSize state')) state'.tabs
                   , pages <- Pages.update applyPageContext updatePage (Pages.Resize (getPagesSize state')) state'.pages
          }

    NextFrame frame ->
      { state | tabs <- Tabs.update applyTabContext updateTab (Tabs.NextFrame frame) state.tabs
              , pages <- Pages.update applyPageContext updatePage (Pages.NextFrame frame) state.pages
      }



view : View tabAction tabState
    -> View pageAction pageState
    -> View (Action tabAction pageAction) (State tabState pageState)
view viewTab viewPage address state =
  let
      containerStyle =
          [ "position" => "absolute"
          , "width"    => toString state.size.x ++ "px"
          , "height"   => toString state.size.y ++ "px"
          , "overflow" => "hidden"
          ]

      tabsSize =
        getTabsSize state

      pagesSize =
        getPagesSize state

      tabsContainerStyle =
          [ "position"  => "absolute"
          , "width"     => toString tabsSize.x ++ "px"
          , "height"    => toString tabsSize.y ++ "px"
          ]

      pagesContainerStyle =
          [ "position" => "absolute"
          , "width" => toString pagesSize.x ++ "px"
          , "height" => toString pagesSize.y ++ "px"
          , "transform" => "translate3d(0px, " ++ toString tabsSize.y ++ "px, 0px)"
          ]

      tabsAddress =
        Signal.forwardTo address TabsAction

      pagesAddress =
        Signal.forwardTo address PagesAction

  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          [ Html.div
                [ Html.Attributes.style tabsContainerStyle ]
                [ Tabs.view viewTab tabsAddress state.tabs ]
          , Html.div
                [ Html.Attributes.style pagesContainerStyle ]
                [ Pages.view viewPage pagesAddress state.pages ]
          ]
