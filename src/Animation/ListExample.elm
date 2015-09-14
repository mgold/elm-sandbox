module Animation.ListExample where

import Html exposing (div, button, text, Html, pre)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp as StartApp
import Signal exposing (Address)
import Animation exposing (Animation, duration, retarget, static, animate)
import Time exposing (Time, second)
import Effects exposing (Effects, Never)
import Task exposing (Task)
import List
import String
import Debug

type PageState = Open | Closed
type SortOrder = SortName | SortId

type alias Item =
  { id : Int
  , name : String
  }

type alias ItemModel =
  { item : Item
  , animation : Animation
  }

type alias Model =
  { sort : SortOrder
  , items : List ItemModel
  , time : Time
  }

init : ( Model, Effects Action )
init =
  ( { sort = SortId
    , items = List.indexedMap itemModel [Item 1 "one", Item 2 "two", Item 3 "three", Item 4 "four", Item 5 "five", Item 6 "six"]
    , time = 0
    }
  , Effects.tick Tick )

itemModel : Int -> Item -> ItemModel
itemModel pos item = { item = item, animation = static (toFloat pos)}

-----------------------------------------------------------------------

type Action
  = Sort SortOrder
  | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Sort order ->
      -- for each one:
      -- get the new position
      -- animate to that new position
      let items = List.indexedMap (updatePosition model.time) <| sortItems order model.items
      in
      ( { model | sort <- order, items <- items }
      , Effects.none )

    Tick t -> -- note that t is the time the program has been running, not the delta since the last tick
      ( { model | time <- t }
      , Effects.tick Tick )


sortItems : SortOrder -> List ItemModel -> List ItemModel
sortItems order items =
  case order of
    SortName -> List.sortBy (.name << .item) items
    SortId -> List.sortBy (.id << .item) items

updatePosition : Time -> Int -> ItemModel -> ItemModel
updatePosition time pos model =
  { model
    | animation <- retarget time (toFloat pos) model.animation |> duration (0.8*second)
  }

-----------------------------------------------------------------------------

itemY : Float -> Float
itemY pos = pos * 50.0

itemView : Address Action -> Time -> ItemModel -> Html
itemView address time model =
  let y = itemY <| animate time model.animation in
  div
    [ style
        [ ("background", "#E6E6EF")
        , ("border", "solid 1px #716DCF")
        , ("borderRightWidth", "10px")
        , ("borderLeftWidth", "0px")
        , ("borderTopWidth", "0px")
        , ("margin", "10px")
        , ("marginLeft", "0px")
        , ("padding", "10px")
        , ("width", "320px")
        , ("position", "absolute")
        , ("top", toString y ++ "px")
        ]
    ]
    [ text model.item.name ]

maxWidth = 200.0

buttonStyle isSelected =
  let highlight =
    if isSelected
    then
      [ ("borderBottomWidth", "4px")
      , ("background", "#FFF6D6")
      ]
    else []
  in
  [ ("color", "#555")
  , ("text-transform", "uppercase")
  , ("cursor", "pointer")
  , ("letter-spacing", "0.15em")
  , ("padding", "8px")
  , ("border", "solid 2px #555")
  , ("background", "white")
  , ("borderRadius", "6px")
  , ("fontWeight", "bold")
  , ("font", "11px 'HelveticaNeue'")
  , ("outline", "none")
  , ("marginRight", "10px")
  ] ++ highlight

view : Address Action -> Model -> Html
view address model =
  div [ style [("margin", "10px")] ]
    [ div [ style [("margin-bottom", "10px")] ]
        [ button
            [ style (buttonStyle (model.sort == SortName))
            , onClick address (Sort SortName) ]
            [ text "Sort Name" ]
        , button
            [ style (buttonStyle (model.sort == SortId))
            , onClick address (Sort SortId) ]
            [ text "Sort Id" ]
        ]
    -- , div
        -- []
        -- (List.map (\model -> pre [] [ text (dump model.transition) ]) model.items)
    , div
        [ style [("position", "relative")] ]
        (List.map (itemView address model.time) model.items)
    ]
------------------------------------------------------------------------------

app =
  StartApp.start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

