module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String.Interpolate exposing (interpolate)
import Time exposing (Time, second)
import List exposing (..)
import Tuple exposing (..)
import Set exposing (..)
import Random exposing (..)
import Keyboard exposing (..)
import Tetromino exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentFigure : PositionedElement
    , nextFigure : Tetromino
    , fallenTiles : Set Tile
    , gameLost : Bool
    }


type alias PositionedElement =
    ( Position, Tetromino )


type Direction
    = Left
    | Down
    | Right


init : ( Model, Cmd Msg )
init =
    ( { currentFigure = ( ( 6, 0 ), zTetromino )
      , nextFigure = lTetromino
      , fallenTiles = Set.empty
      , gameLost = False
      }
    , Random.generate NewFigure randomBlock
    )



-- UPDATE


type Msg
    = Tick Time
    | NewFigure Tetromino
    | Presses Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if not model.gameLost then
        case msg of
            Tick newTime ->
                checkGameLost <| updateCurrentFigure model

            NewFigure shape ->
                updateNextFigure model shape

            Presses code ->
                let
                    direction =
                        case code of
                            37 ->
                                Left

                            39 ->
                                Right

                            -- this is bad
                            _ ->
                                Down
                in
                    if code == 32 then
                        ( { model | currentFigure = ( first model.currentFigure, rotateShape <| Tuple.second model.currentFigure ) }, Cmd.none )
                    else
                        updateCurrentFigurePosition model direction
    else
        ( model, Cmd.none )


updateCurrentFigure : Model -> ( Model, Cmd Msg )
updateCurrentFigure model =
    let
        ( newPosition, element ) =
            updatePosition model.currentFigure Down model.fallenTiles

        ( _, tetromino ) =
            model.currentFigure

        ( _, color ) =
            properties tetromino
    in
        if newPosition == first model.currentFigure then
            ( { model
                | currentFigure = ( ( 6, 0 ), model.nextFigure )
                , fallenTiles = Set.union model.fallenTiles <| Set.fromList <| elementTiles model.currentFigure
              }
            , Random.generate NewFigure randomBlock
            )
        else
            ( { model
                | currentFigure = ( (moveDown (first model.currentFigure)), (Tuple.second model.currentFigure) )
              }
            , Cmd.none
            )


checkGameLost : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkGameLost ( model, msg ) =
    if any (\( ( _, y ), _ ) -> y < 0) <| Set.toList model.fallenTiles then
        ( { model
            | gameLost = True
          }
        , msg
        )
    else
        ( model, msg )


updateNextFigure : Model -> Tetromino -> ( Model, Cmd Msg )
updateNextFigure model shape =
    ( { model | nextFigure = shape }, Cmd.none )


updateCurrentFigurePosition : Model -> Direction -> ( Model, Cmd Msg )
updateCurrentFigurePosition model direction =
    ( { model
        | currentFigure = updatePosition model.currentFigure direction model.fallenTiles
      }
    , Cmd.none
    )


updatePosition : PositionedElement -> Direction -> Set Tile -> PositionedElement
updatePosition positionedElement direction fallenTiles =
    let
        newPositionedElement =
            moveElement positionedElement direction
    in
        if elementExceedsBoard newPositionedElement || elementOnFallenTile newPositionedElement fallenTiles then
            positionedElement
        else
            newPositionedElement


moveElement : PositionedElement -> Direction -> PositionedElement
moveElement ( position, element ) direction =
    case direction of
        Left ->
            ( moveLeft position, element )

        Right ->
            ( moveRight position, element )

        _ ->
            ( moveDown position, element )


elementCanMoveDown : PositionedElement -> Set Tile -> Bool
elementCanMoveDown ( position, element ) fallenTiles =
    (not <| elementExceedsBoard ( position, element ))
        && not (elementOnFallenTile ( position, element ) fallenTiles)


elementExceedsBoard : PositionedElement -> Bool
elementExceedsBoard positionedElement =
    any (\( x, y ) -> y >= 18 || x < 0 || x > 11) <| List.map first <| elementTiles positionedElement


elementOnFallenTile : PositionedElement -> Set Tile -> Bool
elementOnFallenTile positionedElements fallenTiles =
    let
        elementPositions =
            Set.fromList <| List.map first <| elementTiles positionedElements

        tilePositions =
            Set.map first <| fallenTiles
    in
        0 < (Set.size <| Set.intersect tilePositions <| elementPositions)


none : (a -> Bool) -> List a -> Bool
none fn ls =
    not <| any fn ls


moveElementLeft : PositionedElement -> PositionedElement
moveElementLeft ( position, element ) =
    ( (moveLeft position), element )


addPositions : Position -> Position -> Position
addPositions p1 p2 =
    ( (first p1) + (first p2), (Tuple.second p1) + (Tuple.second p2) )


elementTiles : PositionedElement -> List Tile
elementTiles ( position, tetromino ) =
    let
        ( positions, color ) =
            properties tetromino
    in
        List.map (\p -> ( addPositions p position, color )) positions



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


nextFigure : Tetromino -> Html.Html Msg
nextFigure tetromino =
    let
        ( positions, color ) =
            properties tetromino
    in
        svg
            []
            [ g
                [ transform "translate(20 20)"
                , stroke "LightSeaGreen"
                ]
                (List.map (\position -> tile ( position, color )) positions)
            ]


figure : PositionedElement -> Svg Msg
figure ( position, tetromino ) =
    let
        ( positions, color ) =
            properties tetromino
    in
        g
            [ transform (interpolate "translate({0})" [ toPositionString position ])
            ]
            (List.map (\position -> tile ( position, color )) positions)


toPositionString : Position -> String
toPositionString position =
    interpolate "{0} {1}"
        [ (toString <| 10 * first position), (toString <| 10 * Tuple.second position) ]


tiles : Set Tile -> Svg Msg
tiles elements =
    g
        []
        (List.map tile <| Set.toList elements)


tile : Tile -> Svg Msg
tile ( position, color ) =
    rect
        [ x (toString <| 10 * first position), y (toString <| 10 * Tuple.second position), width "8", height "8", fill "white", stroke color ]
        []
