module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import List exposing (..)
import List.Extra exposing (find)
import Set exposing (..)
import Time exposing (..)
import Tetromino exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Figure =
    ( Position, Tetromino )


type alias Model =
    { currentFigure : Figure
    , fallenTiles : Set Tile
    }


boardWidth =
    12


boardHeight =
    18


init : ( Model, Cmd Msg )
init =
    ( { currentFigure = ( ( 5, -2 ), zTetromino )
      , fallenTiles = Set.empty
      }
    , Random.generate ResetTetromino randomTetromino
    )



-- UPDATE


type Direction
    = Left
    | Down
    | Right


type Msg
    = UserInput Int
    | ResetTetromino Tetromino
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetTetromino newTetromino ->
            ( resetCurrentTetromino model newTetromino
            , Cmd.none
            )

        UserInput code ->
            ( moveCurrentFigure model code
            , Cmd.none
            )

        Tick _ ->
            pushDownCurrentFigure model



-- reset current tetromino


resetCurrentTetromino : Model -> Tetromino -> Model
resetCurrentTetromino model tetromino =
    { model
        | currentFigure = ( ( 5, -2 ), tetromino )
    }



-- make tetromino fall Down


pushDownCurrentFigure : Model -> ( Model, Cmd Msg )
pushDownCurrentFigure model =
    let
        ( position, _ ) =
            model.currentFigure

        ( newPosition, element ) =
            updatePosition model.currentFigure Down model.fallenTiles
    in
        if newPosition == position then
            makeCurrentFigureStatic model
        else
            ( { model
                | currentFigure = ( newPosition, element )
              }
            , Cmd.none
            )


makeCurrentFigureStatic : Model -> ( Model, Cmd Msg )
makeCurrentFigureStatic model =
    ( { model
        | fallenTiles =
            Set.union model.fallenTiles <|
                Set.fromList <|
                    tilesOfFigure model.currentFigure
      }
    , Random.generate ResetTetromino randomTetromino
    )



-- moving tetromino


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



-- Update Tetromino Position


updatePosition : Figure -> Direction -> Set Tile -> Figure
updatePosition figure direction fallenTiles =
    let
        newFigure =
            moveElement figure direction
    in
        if validFigurePosition newFigure fallenTiles then
            newFigure
        else
            figure


moveElement : Figure -> Direction -> Figure
moveElement ( position, element ) direction =
    case direction of
        Left ->
            ( moveLeft position, element )

        Right ->
            ( moveRight position, element )

        _ ->
            ( moveDown position, element )


moveDown : Position -> Position
moveDown ( x, y ) =
    ( x, y + 1 )


moveRight : Position -> Position
moveRight ( x, y ) =
    ( x + 1, y )


moveLeft : Position -> Position
moveLeft ( x, y ) =
    ( x - 1, y )



-- rotate Tetromino in place


tryRotateFigure : Figure -> Set Tile -> Figure
tryRotateFigure figure fallenTiles =
    let
        offsetDownFigure =
            rotateFigure figure

        offsetRightFigure =
            moveElement offsetDownFigure Right

        offsetLeftFigure =
            moveElement offsetDownFigure Left
    in
        Maybe.withDefault figure <|
            find (\e -> validFigurePosition e fallenTiles) <|
                [ offsetDownFigure, offsetRightFigure, offsetLeftFigure ]


rotateFigure : Figure -> Figure
rotateFigure ( positions, tetromino ) =
    ( positions, rotateShape tetromino )



-- collision detection


validFigurePosition : Figure -> Set Tile -> Bool
validFigurePosition figure fallenTiles =
    not <|
        figureExceedsBoard figure
            || figureOnFallenTile figure fallenTiles


figureExceedsBoard : Figure -> Bool
figureExceedsBoard figure =
    any tileWithinBoard <|
        tilesOfFigure figure


tileWithinBoard : Tile -> Bool
tileWithinBoard ( ( x, y ), _ ) =
    y >= boardHeight || x < 0 || x >= boardWidth


figureOnFallenTile : Figure -> Set Tile -> Bool
figureOnFallenTile figure fallenTiles =
    let
        elementPositions =
            Set.fromList <| List.map Tuple.first <| tilesOfFigure figure

        tilePositions =
            Set.map Tuple.first <| fallenTiles
    in
        0 < (Set.size <| Set.intersect tilePositions <| elementPositions)



-- convert a positioned tetromino to its positioned tiles


tilesOfFigure : Figure -> List Tile
tilesOfFigure ( position, tetromino ) =
    let
        ( positions, color ) =
            tetromino
    in
        List.map (\p -> ( addPositions p position, color )) positions


addPositions : Position -> Position -> Position
addPositions ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second / 2) Tick
        , Keyboard.downs UserInput
        ]



-- VIEW


type alias Tile =
    ( Position, Color )


tileWidth =
    20


view : Model -> Html.Html Msg
view model =
    svg
        [ width "90%", height "90%" ]
        [ figure model.currentFigure
        , tiles model.fallenTiles
        , boardBorder
        ]


boardBorder : Svg Msg
boardBorder =
    let
        w =
            toString <| boardWidth * tileWidth

        h =
            toString <| boardHeight * tileWidth
    in
        rect
            [ x "0"
            , y "0"
            , width w
            , height h
            , fill "none"
            , stroke "mediumslateblue"
            , strokeWidth "2"
            ]
            []


figure : Figure -> Svg Msg
figure figure =
    g
        []
        (List.map tile <| tilesOfFigure figure)


tiles : Set Tile -> Svg Msg
tiles ts =
    g [] <|
        List.map tile <|
            Set.toList ts


tile : Tile -> Svg Msg
tile ( ( tileX, tileY ), color ) =
    g
        [ fill "white", stroke color, strokeWidth "2" ]
        [ rect
            [ x (toString <| tileWidth * (toFloat tileX) + (tileWidth * 0.1))
            , y (toString <| tileWidth * (toFloat tileY) + (tileWidth * 0.1))
            , width (toString <| tileWidth - (tileWidth * 0.2))
            , height (toString <| tileWidth - (tileWidth * 0.2))
            ]
            []
        , g
            [ opacity "0.7" ]
            [ line
                [ x1 (toString <| tileWidth * (toFloat tileX) + tileWidth * 0.3)
                , y1 (toString <| tileWidth * (toFloat tileY) + tileWidth * 0.7)
                , x2 (toString <| tileWidth * (toFloat tileX) + tileWidth * 0.7)
                , y2 (toString <| tileWidth * (toFloat tileY) + tileWidth * 0.7)
                ]
                []
            , line
                [ x1 (toString <| tileWidth * (toFloat tileX) + tileWidth * 0.7)
                , y1 (toString <| tileWidth * (toFloat tileY) + tileWidth * 0.3)
                , x2 (toString <| tileWidth * (toFloat tileX) + tileWidth * 0.7)
                , y2 (toString <| tileWidth * (toFloat tileY) + tileWidth * 0.7)
                ]
                []
            ]
        ]
