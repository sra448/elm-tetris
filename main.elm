import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String.Interpolate exposing(interpolate)
import Time exposing (Time, second)
import Tuple exposing (..)
import Set exposing (..)


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
  , fallenTiles : Set Position
  , score : Int
  }


type alias Position = ( Int, Int )


type alias Figure = List Position


type alias PositionedElement = ( Position, Figure )


init : (Model, Cmd Msg)
init =
  ({ time = 0
  , currentFigure = ( ( 6, 0 ), zThingy )
  , fallenTiles = Set.empty
  , score = 0
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


toPositionString : Position -> String
toPositionString position =
  interpolate "{0} {1}"
    [(toString <| 10 * first position), (toString <| 10 * Tuple.second position)]



-- UPDATE


type Msg =
  Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (updateCurrentFigure model, Cmd.none)



updateCurrentFigure : Model -> Model
updateCurrentFigure model =
  if elementCanMoveDown model.currentFigure model.fallenTiles then
    { model |
      time = model.time + 1,
      currentFigure = ( (moveDown (first model.currentFigure)), (Tuple.second model.currentFigure) )
    }
  else
    { model |
      time = model.time + 1,
      currentFigure = ( ( 6, 0 ), (Tuple.second model.currentFigure) ),
      fallenTiles = Set.union model.fallenTiles <| Set.fromList <| elementPositions model.currentFigure
    }


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
  svg
    [ width "120", height "180" ]
    [ figure model.currentFigure
    , tiles model.fallenTiles
    , rect [ x "0", y "0", width "120", height "180", fill "none", stroke "black" ] []
    ]


figure : PositionedElement -> Svg Msg
figure ( position, elements ) =
  g
    [ transform (interpolate "translate({0})" [ toPositionString position ])
    , stroke "LightSeaGreen"
    ]
    <| List.map tile elements


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



-- threePointyThingy : Position -> Html.Html Msg
-- threePointyThingy t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "LightSeaGreen"
--     ]
--     [ tile ( 0, 0 )
--     , tile ( 10, 0 )
--     , tile ( 20, 0 )
--     , tile ( 10, 10 )
--     ]
--
--
-- crowBar : Position -> Html.Html Msg
-- crowBar t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "fuchsia"
--     ]
--     [ tile ( 0, 0 )
--     , tile ( 0, 10 )
--     , tile ( 0, 20 )
--     , tile ( 0, 30 )
--     ]
--
--
-- zThingy : Position -> Html.Html Msg
-- zThingy t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "crimson"
--     ]
--     [ tile ( 0, 0 )
--     , tile ( 10, 0 )
--     , tile ( 10, 10 )
--     , tile ( 20, 10 )
--     ]
--
--
-- zThingyReverse : Position -> Html.Html Msg
-- zThingyReverse t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "midnightblue"
--     ]
--     [ tile ( 0, 10 )
--     , tile ( 10, 10 )
--     , tile ( 10, 0 )
--     , tile ( 20, 0 )
--     ]
--
--
-- lThingy : Position -> Html.Html Msg
-- lThingy t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "LightCoral"
--     ]
--     [ tile ( 0, 0 )
--     , tile ( 10, 0 )
--     , tile ( 20, 00 )
--     , tile ( 20, 10 )
--     ]
--
--
-- lThingyReverse : Position -> Html.Html Msg
-- lThingyReverse t =
--   g
--     [ transform (interpolate "translate({0} {1})" [(toString <| 10 * first t), (toString <| 10 * Tuple.second t)])
--     , stroke "Coral"
--     ]
--     [ tile ( 0, 10 )
--     , tile ( 0, 0 )
--     , tile ( 10, 0 )
--     , tile ( 20, 0 )
--     ]
