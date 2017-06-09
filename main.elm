import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
import Time exposing (Time, second)
import List exposing (..)
import Tuple exposing (..)
import Set exposing (..)
import Random exposing (..)
import Keyboard exposing (..)



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


init : (Model, Cmd Msg)
init =
  ({ time = 0
  , currentFigure = ( ( 6, 0 ), zThingy )
  , nextFigure = lThingy
  , fallenTiles = Set.empty
  }, Random.generate NewFigure (Random.int 1 4))


moveDown : Position -> Position
moveDown ( x, y ) =
  ( x, y + 1 )


moveRight : Position -> Position
moveRight ( x, y ) =
  ( x + 1, y )


moveLeft : Position -> Position
moveLeft ( x, y ) =
  ( x - 1, y )


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



-- UPDATE


type Msg =
  Tick Time
  | NewFigure Int
  | Presses Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      updateCurrentFigure model
    NewFigure nr ->
      updateNextFigure model nr
    Presses code ->
      updateCurrentFigurePosition model code



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


updateCurrentFigurePosition : Model -> Int -> (Model, Cmd Msg)
updateCurrentFigurePosition model code =
  case code of
    37 ->
      ({ model |
        currentFigure = tryMoveElementLeft model.currentFigure
      }, Cmd.none)
    39 ->
      ({ model |
        currentFigure = tryMoveElementRight model.currentFigure
      }, Cmd.none)
    40 ->
      ({ model |
        currentFigure = tryMoveElementDown model.currentFigure model.fallenTiles
      }, Cmd.none)
    _ ->
      (model, Cmd.none)



elementCanMoveDown : PositionedElement -> Set Position -> Bool
elementCanMoveDown (position, element) fallenTiles =
  (not <| elementExceedsBoard (position, element))
    && (elementOnFallenTile (position, element) fallenTiles)


elementExceedsBoard : PositionedElement -> Bool
elementExceedsBoard positionedElement =
  any (\(_, y) -> y >= 17) <| elementPositions positionedElement


elementOnFallenTile : PositionedElement -> Set Position -> Bool
elementOnFallenTile  currentElement fallenTiles =
  0 == (Set.size <| Set.intersect fallenTiles <| Set.fromList <| elementPositions <| moveElementDown currentElement)


tryMoveElementDown : PositionedElement -> Set Position -> PositionedElement
tryMoveElementDown positionedElement fallenTiles =
  let
    newPositionedElement = moveElementDown positionedElement
  in
    if elementCanMoveDown positionedElement fallenTiles then
      newPositionedElement
    else
      positionedElement


moveElementDown : PositionedElement -> PositionedElement
moveElementDown (position, element) =
  ( (moveDown position), element )


tryMoveElementRight : PositionedElement -> PositionedElement
tryMoveElementRight positionedElement =
  let
    newPositionedElement = moveElementRight positionedElement
  in
    if none (\(x, _) -> x > 11) <| elementPositions newPositionedElement then
      newPositionedElement
    else
      positionedElement


moveElementRight : PositionedElement -> PositionedElement
moveElementRight (position, element) =
  ( (moveRight position), element )


none fn ls = not <| any fn ls


tryMoveElementLeft : PositionedElement -> PositionedElement
tryMoveElementLeft positionedElement =
  let
    newPositionedElement = moveElementLeft positionedElement
  in
    if none (\(x, _) -> x < 0) <| elementPositions newPositionedElement then
      newPositionedElement
    else
      positionedElement


moveElementLeft : PositionedElement -> PositionedElement
moveElementLeft (position, element) =
  ( (moveLeft position), element )



addPositions : Position -> Position -> Position
addPositions p1 p2 =
  ( (first p1) + (first p2), (Tuple.second p1) + (Tuple.second p2) )


elementPositions : PositionedElement -> List Position
elementPositions (position, element) =
  List.map (\p -> addPositions p position) element



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (Time.second / 2) Tick
    , Keyboard.downs Presses
    ]



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


toPositionString : Position -> String
toPositionString position =
  interpolate "{0} {1}"
    [(toString <| 10 * first position), (toString <| 10 * Tuple.second position)]


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
