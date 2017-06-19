module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import List exposing (..)
import List.Extra exposing (find)
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


tileWidth =
    20


boardWidth =
    12


boardHeight =
    18


type alias Tile =
    ( Position, Color )


type alias Figure =
    ( Position, Tetromino )


type Direction
    = Left
    | Down
    | Right


type alias Model =
    { currentFigure : Figure
    , nextTetromino : Tetromino
    , fallenTiles : Set Tile
    , gameLost : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { currentFigure = ( ( 6, -2 ), zTetromino )
      , nextTetromino = lTetromino
      , fallenTiles = Set.empty
      , gameLost = False
      }
    , Random.generate ResetNextTetromino randomBlock
    )



-- UPDATE


type Msg
    = ResetNextTetromino Tetromino
    | UserInput Int
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetNextTetromino tetromino ->
            ( { model | nextTetromino = tetromino }, Cmd.none )

        UserInput code ->
            ( moveCurrentFigure model code, Cmd.none )

        Tick _ ->
            checkGameLost <|
                removeFullRows <|
                    pushDownCurrentFigure model


moveCurrentFigure : Model -> Int -> Model
moveCurrentFigure model code =
    { model
        | currentFigure =
            case code of
                37 ->
                    updatePosition model.currentFigure Left model.fallenTiles

                39 ->
                    updatePosition model.currentFigure Right model.fallenTiles

                40 ->
                    updatePosition model.currentFigure Down model.fallenTiles

                32 ->
                    tryRotateFigure model.currentFigure model.fallenTiles

                _ ->
                    model.currentFigure
    }


tryRotateFigure : Figure -> Set Tile -> Figure
tryRotateFigure figure fallenTiles =
    performWithWallKick
        (\( position, tetromino ) ->
            ( position, rotateShape tetromino )
        )
        figure
        fallenTiles


updateCurrentFigurePosition : Model -> Direction -> Figure
updateCurrentFigurePosition model direction =
    updatePosition model.currentFigure direction model.fallenTiles


pushDownCurrentFigure : Model -> ( Model, Cmd Msg )
pushDownCurrentFigure model =
    let
        ( position, _ ) =
            model.currentFigure

        ( newPosition, element ) =
            updatePosition model.currentFigure Down model.fallenTiles
    in
        if newPosition == position then
            ( { model
                | currentFigure = ( ( 6, 0 ), model.nextTetromino )
                , fallenTiles =
                    Set.union model.fallenTiles <|
                        Set.fromList <|
                            tilesOccupiedByFigure model.currentFigure
              }
            , Random.generate ResetNextTetromino randomBlock
            )
        else
            ( { model
                | currentFigure = ( newPosition, element )
              }
            , Cmd.none
            )


removeFullRows : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
removeFullRows ( model, msg ) =
    let
        rows =
            List.map (tilesInRow model.fallenTiles) <|
                List.range 1 boardHeight

        ( scoreMultiplier, fallenTiles ) =
            List.foldl
                (\tiles ( removedRows, remainingTiles ) ->
                    if Set.size tiles == boardWidth then
                        ( removedRows + 1, remainingTiles )
                    else
                        ( removedRows
                        , Set.union
                            (Set.map
                                (\( ( x, y ), color ) ->
                                    ( ( x, y + removedRows ), color )
                                )
                                tiles
                            )
                            remainingTiles
                        )
                )
                ( 0, Set.empty )
                (List.reverse rows)
    in
        ( { model | fallenTiles = fallenTiles }
        , msg
        )


tilesInRow : Set Tile -> Int -> Set Tile
tilesInRow fallenTiles i =
    Set.filter (\( ( _, y ), _ ) -> y == i) fallenTiles


performWithWallKick : (Figure -> Figure) -> Figure -> Set Tile -> Figure
performWithWallKick transformFn figure fallenTiles =
    let
        newFigure =
            transformFn figure

        offsetRight =
            moveElement newFigure Right

        offsetLeft =
            moveElement newFigure Left

        element =
            find (\e -> validElementPosition e fallenTiles) <|
                [ newFigure, offsetRight, offsetLeft ]
    in
        case element of
            Nothing ->
                figure

            Just a ->
                a


validElementPosition : Figure -> Set Tile -> Bool
validElementPosition figure fallenTiles =
    not <|
        elementExceedsBoard figure
            || elementOnFallenTile figure fallenTiles


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


updatePosition : Figure -> Direction -> Set Tile -> Figure
updatePosition figure direction fallenTiles =
    let
        newFigure =
            moveElement figure direction
    in
        if
            elementExceedsBoard newFigure
                || elementOnFallenTile newFigure fallenTiles
        then
            figure
        else
            newFigure


moveElement : Figure -> Direction -> Figure
moveElement ( position, element ) direction =
    case direction of
        Left ->
            ( moveLeft position, element )

        Right ->
            ( moveRight position, element )

        _ ->
            ( moveDown position, element )


elementCanMoveDown : Figure -> Set Tile -> Bool
elementCanMoveDown ( position, element ) fallenTiles =
    (not <| elementExceedsBoard ( position, element ))
        && not (elementOnFallenTile ( position, element ) fallenTiles)


elementExceedsBoard : Figure -> Bool
elementExceedsBoard figure =
    any tileWithinBoard <|
        List.map first <|
            tilesOccupiedByFigure figure


tileWithinBoard : Position -> Bool
tileWithinBoard ( x, y ) =
    y >= boardHeight || x < 0 || x >= boardWidth


elementOnFallenTile : Figure -> Set Tile -> Bool
elementOnFallenTile positionedElements fallenTiles =
    let
        elementPositions =
            Set.fromList <| List.map first <| tilesOccupiedByFigure positionedElements

        tilePositions =
            Set.map first <| fallenTiles
    in
        0 < (Set.size <| Set.intersect tilePositions <| elementPositions)


tilesOccupiedByFigure : Figure -> List Tile
tilesOccupiedByFigure ( position, tetromino ) =
    let
        ( positions, color ) =
            tetromino
    in
        List.map (\p -> ( addPositions p position, color )) positions


addPositions : Position -> Position -> Position
addPositions p1 p2 =
    ( (first p1) + (first p2), (Tuple.second p1) + (Tuple.second p2) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second / 2) Tick
        , Keyboard.downs UserInput
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    div
        [ Html.Attributes.style [ ( "padding", "20" ) ] ]
        [ Html.text "ELM Tetris"
        , board model
        , nextTetromino model.nextTetromino
        ]


board : Model -> Html.Html Msg
board model =
    let
        w =
            toString <| boardWidth * tileWidth

        h =
            toString <| boardHeight * tileWidth
    in
        svg
            [ width w, height h ]
            [ figure model.currentFigure
            , tiles model.fallenTiles
            , rect [ x "0", y "0", width w, height h, fill "none", stroke "mediumslateblue", strokeWidth "2" ] []
            ]


nextTetromino : Tetromino -> Html.Html Msg
nextTetromino tetromino =
    let
        ( positions, color ) =
            tetromino
    in
        svg
            []
            [ g
                [ transform "translate(20 20)" ]
                (List.map (\position -> tile ( position, color )) positions)
            ]


figure : Figure -> Svg Msg
figure figure =
    g
        []
        (List.map tile <| tilesOccupiedByFigure figure)


toPositionString : Position -> String
toPositionString position =
    (toString <| tileWidth * first position)
        ++ (toString <| tileWidth * Tuple.second position)


tiles : Set Tile -> Svg Msg
tiles elements =
    g
        []
        (List.map tile <| Set.toList elements)


tile : Tile -> Svg Msg
tile ( position, color ) =
    g
        [ fill "white", stroke color, strokeWidth "2" ]
        [ rect
            [ x (toString <| tileWidth * (toFloat <| first position) + (tileWidth * 0.1))
            , y (toString <| tileWidth * (toFloat <| Tuple.second position) + (tileWidth * 0.1))
            , width (toString <| tileWidth - (tileWidth * 0.2))
            , height (toString <| tileWidth - (tileWidth * 0.2))
            ]
            []
        , g
            [ opacity "0.7" ]
            [ line
                [ x1 (toString <| tileWidth * (toFloat <| first position) + tileWidth * 0.3)
                , y1 (toString <| tileWidth * (toFloat <| Tuple.second position) + tileWidth * 0.7)
                , x2 (toString <| tileWidth * (toFloat <| first position) + tileWidth * 0.7)
                , y2 (toString <| tileWidth * (toFloat <| Tuple.second position) + tileWidth * 0.7)
                ]
                []
            , line
                [ x1 (toString <| tileWidth * (toFloat <| first position) + tileWidth * 0.7)
                , y1 (toString <| tileWidth * (toFloat <| Tuple.second position) + tileWidth * 0.3)
                , x2 (toString <| tileWidth * (toFloat <| first position) + tileWidth * 0.7)
                , y2 (toString <| tileWidth * (toFloat <| Tuple.second position) + tileWidth * 0.7)
                ]
                []
            ]
        ]
