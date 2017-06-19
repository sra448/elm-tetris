module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String.Interpolate exposing (interpolate)
import Time exposing (Time, second)
import List exposing (..)
import List.Extra exposing (find, group)
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


type alias PositionedElement =
    ( Position, Tetromino )


type Direction
    = Left
    | Down
    | Right


type alias Model =
    { currentFigure : PositionedElement
    , nextFigure : Tetromino
    , fallenTiles : Set Tile
    , gameLost : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { currentFigure = ( ( 6, 0 ), zTetromino )
      , nextFigure = lTetromino
      , fallenTiles = Set.empty
      , gameLost = False
      }
    , Random.generate RandomTetromino randomBlock
    )



-- UPDATE


type Msg
    = RandomTetromino Tetromino
    | UserInput Int
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomTetromino shape ->
            ( { model | nextFigure = shape }, Cmd.none )

        UserInput code ->
            ( updateFigureByUser model code, Cmd.none )

        Tick _ ->
            checkGameLost <|
                removeFullRows <|
                    updateCurrentFigure model


updateFigureByUser : Model -> Int -> Model
updateFigureByUser model code =
    let
        direction =
            case code of
                37 ->
                    Left

                39 ->
                    Right

                _ ->
                    Down
    in
        if code == 32 then
            { model
                | currentFigure = rotateCurrentFigure model.currentFigure model.fallenTiles
            }
        else
            { model
                | currentFigure = updateCurrentFigurePosition model direction
            }


rotateCurrentFigure : PositionedElement -> Set Tile -> PositionedElement
rotateCurrentFigure positionedElement fallenTiles =
    performWithWallKick (\e -> ( first e, rotateShape <| Tuple.second e )) positionedElement fallenTiles


updateCurrentFigurePosition : Model -> Direction -> PositionedElement
updateCurrentFigurePosition model direction =
    updatePosition model.currentFigure direction model.fallenTiles


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
            , Random.generate RandomTetromino randomBlock
            )
        else
            ( { model
                | currentFigure = ( (moveDown (first model.currentFigure)), (Tuple.second model.currentFigure) )
              }
            , Cmd.none
            )


performWithWallKick : (PositionedElement -> PositionedElement) -> PositionedElement -> Set Tile -> PositionedElement
performWithWallKick transformElement positionedElement fallenTiles =
    let
        newPositionedElement =
            transformElement positionedElement

        offsetRight =
            moveElement newPositionedElement Right

        offsetLeft =
            moveElement newPositionedElement Left

        element =
            find (\e -> validElementPosition e fallenTiles) <|
                [ newPositionedElement, offsetRight, offsetLeft ]
    in
        case element of
            Nothing ->
                positionedElement

            Just a ->
                a


validElementPosition : PositionedElement -> Set Tile -> Bool
validElementPosition positionedElement fallenTiles =
    not <| elementExceedsBoard positionedElement || elementOnFallenTile positionedElement fallenTiles


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
    any (\( x, y ) -> y >= boardHeight || x < 0 || x >= boardWidth) <| List.map first <| elementTiles positionedElement


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
        , Keyboard.downs UserInput
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    div
        [ Html.Attributes.style [ ( "padding", "20" ) ] ]
        [ Html.text "ELM Tetris"
        , board model
        , nextFigure model.nextFigure
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


nextFigure : Tetromino -> Html.Html Msg
nextFigure tetromino =
    let
        ( positions, color ) =
            properties tetromino
    in
        svg
            []
            [ g
                [ transform "translate(20 20)" ]
                (List.map (\position -> tile ( position, color )) positions)
            ]


figure : PositionedElement -> Svg Msg
figure positionedElement =
    g
        []
        (List.map tile <| elementTiles positionedElement)


toPositionString : Position -> String
toPositionString position =
    interpolate "{0} {1}"
        [ (toString <| tileWidth * first position), (toString <| tileWidth * Tuple.second position) ]


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
