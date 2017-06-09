import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
import Time exposing (Time, second)
import Tuple exposing (..)
import Set exposing (..)
import Random exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { time : Int
  , currentFigure : PositionedElement
  , nextFigure : Figure
  , fallenTiles : Set Position
  }


type alias Position = ( Int, Int )


type alias Figure = List Position


type alias PositionedElement = ( Position, Figure )


type Figures =
  LThingy | ZThingy


init : (Model, Cmd Msg)
init =
  ({ time = 0
  , currentFigure = ( ( 6, 0 ), zThingy )
  , nextFigure = lThingy
  , fallenTiles = Set.empty
  }, Cmd.none)


moveDown : Position -> Position
moveDown position =
  ( (first position), (Tuple.second position) + 1 )


lThingy : List Position
lThingy =
  [ ( -1, 0 )
  , ( 0, 0 )
  , ( 1, 0 )
  , ( 1, 1 )
  ]


zThingy : List Position
zThingy =
  [ ( -1, -2 )
  , ( 0, -2 )
  , ( 0, -1 )
  , ( 1, -1 )
  ]


iThingy : List Position
iThingy =
  [ ( 0, -2 )
  , ( 0, -1 )
  , ( 0, 0 )
  , ( 0, 1 )
  ]


blockThingy : List Position
blockThingy =
  [ ( -1, -1 )
  , ( 0, -1 )
  , ( 0, 0 )
  , ( -1, 0 )
  ]


toPositionString : Position -> String
toPositionString position =
  interpolate "{0} {1}"
    [(toString <| 10 * first position), (toString <| 10 * Tuple.second position)]



-- UPDATE


type Msg =
  Tick Time
  | NewFigure Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      updateCurrentFigure model
    NewFigure nr ->
      updateNextFigure model nr



updateCurrentFigure : Model -> (Model, Cmd Msg)
updateCurrentFigure model =
  if elementCanMoveDown model.currentFigure model.fallenTiles then
    ({ model |
      time = model.time + 1,
      currentFigure = ( (moveDown (first model.currentFigure)), (Tuple.second model.currentFigure) )
    }, Cmd.none)
  else
    ({ model |
      time = model.time + 1,
      currentFigure = ( ( 6, 0 ), model.nextFigure ),
      fallenTiles = Set.union model.fallenTiles <| Set.fromList <| elementPositions model.currentFigure
    }, Random.generate NewFigure (Random.int 1 4))


updateNextFigure : Model -> Int -> (Model, Cmd Msg)
updateNextFigure model nr =
  case nr of
    1 -> ({ model | nextFigure = zThingy }, Cmd.none)
    2 -> ({ model | nextFigure = blockThingy }, Cmd.none)
    3 -> ({ model | nextFigure = iThingy }, Cmd.none)
    _ -> ({ model | nextFigure = lThingy }, Cmd.none)


elementCanMoveDown : PositionedElement -> Set Position -> Bool
elementCanMoveDown (position, element) fallenTiles =
  (Tuple.second position) < 18 && (elementOnFallenTile (position, element) fallenTiles)


elementOnFallenTile : PositionedElement -> Set Position -> Bool
elementOnFallenTile  currentElement fallenTiles =
  0 == (Set.size <| Set.intersect fallenTiles <| Set.fromList <| elementPositions <| moveElementDown currentElement)


moveElementDown : PositionedElement -> PositionedElement
moveElementDown (position, element) =
  ( (moveDown position), element )



addPositions : Position -> Position -> Position
addPositions p1 p2 =
  ( (first p1) + (first p2), (Tuple.second p1) + (Tuple.second p2) )


elementPositions : PositionedElement -> List Position
elementPositions (position, element) =
  List.map (\p -> addPositions p position) element



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.second / 2) Tick



-- VIEW


view : Model -> Html.Html Msg
view model =
  div
    []
    [ Html.text "ELM Tetris"
    , board model
    , nextFigure model.nextFigure
    ]



board : Model -> Html.Html Msg
board model =
  svg
    [ width "120", height "180" ]
    [ figure model.currentFigure
    , tiles model.fallenTiles
    , rect [ x "0", y "0", width "120", height "180", fill "none", stroke "black" ] []
    ]


nextFigure : Figure -> Html.Html Msg
nextFigure figure =
  svg
    []
    [ g
      [ transform "translate(20 20)"
      , stroke "LightSeaGreen"
      ]
      (List.map tile figure)
    ]


figure : PositionedElement -> Svg Msg
figure ( position, elements ) =
  g
    [ transform (interpolate "translate({0})" [ toPositionString position ])
    , stroke "LightSeaGreen"
    ]
    (List.map tile elements)


tiles : Set Position -> Svg Msg
tiles ts =
  g
    [ stroke "crimson" ]
    (List.map tile <| Set.toList ts)


tile : Position -> Svg Msg
tile position =
  rect
    [ x (toString <| 10 * first position), y (toString <| 10 * Tuple.second position), width "8", height "8", fill "white" ]
    []
