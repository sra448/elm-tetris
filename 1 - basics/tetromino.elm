module Tetromino exposing (..)

import List exposing (..)
import Tuple exposing (..)
import Set exposing (..)
import Random exposing (..)


-- MODEL


type alias Position =
    ( Int, Int )


type alias Color =
    String


type alias Shape =
    List Position


type alias Tetromino =
    ( Shape, Color )


randomBlock : Generator Tetromino
randomBlock =
    Random.map tetrominoById <| Random.int 1 7


tetrominoById : Int -> Tetromino
tetrominoById i =
    case i of
        1 ->
            lTetromino

        2 ->
            zTetromino

        3 ->
            iTetromino

        4 ->
            sTetromino

        5 ->
            tTetromino

        6 ->
            jTetromino

        _ ->
            oTetromino


lTetromino : Tetromino
lTetromino =
    ( [ ( 0, 1 )
      , ( 1, 1 )
      , ( 2, 1 )
      , ( 2, 0 )
      ]
    , "crimson"
    )


jTetromino : Tetromino
jTetromino =
    ( [ ( 0, 0 )
      , ( 0, 1 )
      , ( 1, 1 )
      , ( 2, 1 )
      ]
    , "teal"
    )


zTetromino : Tetromino
zTetromino =
    ( [ ( 0, 1 )
      , ( 1, 1 )
      , ( 1, 0 )
      , ( 2, 0 )
      ]
    , "coral"
    )


sTetromino : Tetromino
sTetromino =
    ( [ ( 0, 0 )
      , ( 1, 1 )
      , ( 1, 0 )
      , ( 2, 1 )
      ]
    , "palevioletred"
    )


tTetromino : Tetromino
tTetromino =
    ( [ ( 0, 1 )
      , ( 1, 1 )
      , ( 1, 0 )
      , ( 2, 1 )
      ]
    , "olivedrab"
    )


iTetromino : Tetromino
iTetromino =
    ( [ ( 0, 1 )
      , ( 1, 1 )
      , ( 2, 1 )
      , ( 3, 1 )
      ]
    , "lightseagreen"
    )


oTetromino : Tetromino
oTetromino =
    ( [ ( 0, 0 )
      , ( 0, 1 )
      , ( 1, 0 )
      , ( 1, 1 )
      ]
    , "goldenrod"
    )


rotateShape : Tetromino -> Tetromino
rotateShape ( positions, color ) =
    let
        isEven =
            2 == List.foldl (\( x, y ) acc -> max acc <| max x y) 0 positions
    in
        if isEven then
            ( List.map rotateEven <| positions, color )
        else
            ( List.map rotateUneven <| positions, color )


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
