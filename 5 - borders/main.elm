module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import List exposing (..)
import Tetromino exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias PositionedTetromino =
    ( Position, Tetromino )


type alias Model =
    { currentFigure : PositionedTetromino
    }


boardWidth =
    12


boardHeight =
    18


init : ( Model, Cmd Msg )
init =
    ( { currentFigure = ( ( 5, -2 ), zTetromino ) }
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



-- reset current tetromino


resetCurrentTetromino : Model -> Tetromino -> Model
resetCurrentTetromino model tetromino =
    { model
        | currentFigure = ( ( 5, -2 ), tetromino )
    }



-- moving tetromino


moveCurrentFigure : Model -> Int -> Model
moveCurrentFigure model code =
    { model
        | currentFigure =
            case code of
                37 ->
                    updatePosition model.currentFigure Left

                39 ->
                    updatePosition model.currentFigure Right

                40 ->
                    updatePosition model.currentFigure Down

                32 ->
                    rotateFigure model.currentFigure

                _ ->
                    model.currentFigure
    }



-- Update Tetromino Position


updatePosition : PositionedTetromino -> Direction -> PositionedTetromino
updatePosition figure direction =
    let
        newFigure =
            moveElement figure direction
    in
        if figureExceedsBoard newFigure then
            figure
        else
            newFigure


moveElement : PositionedTetromino -> Direction -> PositionedTetromino
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


rotateFigure : PositionedTetromino -> PositionedTetromino
rotateFigure ( positions, tetromino ) =
    ( positions, rotateShape tetromino )



-- collision detection


figureExceedsBoard : PositionedTetromino -> Bool
figureExceedsBoard figure =
    any tileWithinBoard <|
        tilesOfFigure figure


tileWithinBoard : Tile -> Bool
tileWithinBoard ( ( x, y ), _ ) =
    y >= boardHeight || x < 0 || x >= boardWidth



-- convert a positioned tetromino to its positioned tiles


tilesOfFigure : PositionedTetromino -> List Tile
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
    Keyboard.downs UserInput



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


figure : PositionedTetromino -> Svg Msg
figure figure =
    g
        []
        (List.map tile <| tilesOfFigure figure)


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
