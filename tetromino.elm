module Tetromino exposing (..)

import List exposing (..)
import Tuple exposing (..)
import Set exposing (..)
import Random exposing (..)


-- MODEL


type alias Position =
    ( Int, Int )


type alias Tile =
    ( Position, Color )


type alias Tetromino =
    ( List Position, Color )


type alias Color =
    String


moveDown : Position -> Position
moveDown ( x, y ) =
    ( x, y + 1 )


moveRight : Position -> Position
moveRight ( x, y ) =
    ( x + 1, y )


moveLeft : Position -> Position
moveLeft ( x, y ) =
    ( x - 1, y )


randomBlock : Generator Tetromino
randomBlock =
    Random.map blockById <| Random.int 1 4


blockById : Int -> Tetromino
blockById i =
    case i of
        1 ->
            lTetromino

        2 ->
            zTetromino

        3 ->
            iTetromino

        _ ->
            oTetromino


lTetromino : Tetromino
lTetromino =
    ( [ ( -1, -1 )
      , ( 0, -1 )
      , ( 1, -1 )
      , ( 1, 0 )
      ]
    , "crimson"
    )


zTetromino : Tetromino
zTetromino =
    ( [ ( -1, -1 )
      , ( 0, -1 )
      , ( 0, 0 )
      , ( 1, 0 )
      ]
    , "coral"
    )


iTetromino : Tetromino
iTetromino =
    ( [ ( -1, -2 )
      , ( -1, -1 )
      , ( -1, 0 )
      , ( -1, 1 )
      ]
    , "lightseagreen"
    )


oTetromino : Tetromino
oTetromino =
    ( [ ( -1, -1 )
      , ( 0, -1 )
      , ( 0, 0 )
      , ( -1, 0 )
      ]
    , "goldenrod"
    )


rotateShape : Tetromino -> Tetromino
rotateShape ( positions, color ) =
    case color of
        "crimson" ->
            ( List.map (rotateEven) <| positions, color )

        "lightseagreen" ->
            ( List.map (rotateEven) <| positions, color )

        "coral" ->
            ( List.map (rotateUneven) <| positions, color )

        _ ->
            ( List.map (rotateUneven) <| positions, color )


rotateEven : Position -> Position
rotateEven position =
    case position of
        ( -2, -2 ) ->
            ( 1, -2 )

        ( -1, -2 ) ->
            ( 1, -1 )

        ( 0, -2 ) ->
            ( 1, 0 )

        ( 1, -2 ) ->
            ( 1, 1 )

        ( -2, -1 ) ->
            ( 0, -2 )

        ( -1, -1 ) ->
            ( 0, -1 )

        ( 0, -1 ) ->
            ( 0, 0 )

        ( 1, -1 ) ->
            ( 0, 1 )

        ( -2, 0 ) ->
            ( -1, -2 )

        ( -1, 0 ) ->
            ( -1, -1 )

        ( 0, 0 ) ->
            ( -1, 0 )

        ( 1, 0 ) ->
            ( -1, 1 )

        ( -2, 1 ) ->
            ( -2, -2 )

        ( -1, 1 ) ->
            ( -2, -1 )

        ( 0, 1 ) ->
            ( -2, 0 )

        ( 1, 1 ) ->
            ( -2, 1 )

        ( a, b ) ->
            ( a, b )


rotateUneven : Position -> Position
rotateUneven position =
    case position of
        ( -1, -1 ) ->
            ( 1, -1 )

        ( 0, -1 ) ->
            ( 1, 0 )

        ( 1, -1 ) ->
            ( 1, 1 )

        ( -1, 0 ) ->
            ( 0, -1 )

        ( 0, 0 ) ->
            ( 0, 0 )

        ( 1, 0 ) ->
            ( 0, 1 )

        ( -1, 1 ) ->
            ( -1, -1 )

        ( 0, 1 ) ->
            ( -1, 0 )

        ( 1, 1 ) ->
            ( -1, 1 )

        ( a, b ) ->
            ( a, b )
