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


type alias Color =
    String


type alias Shape =
    List Position


type Tetromino
    = ZTetromino Shape Color
    | STetromino Shape Color
    | TTetromino Shape Color
    | ITetromino Shape Color
    | OTetromino Shape Color
    | LTetromino Shape Color
    | JTetromino Shape Color


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
    Random.map blockById <| Random.int 1 7


blockById : Int -> Tetromino
blockById i =
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
    LTetromino
        [ ( -1, -1 )
        , ( 0, -1 )
        , ( 1, -1 )
        , ( 1, 0 )
        ]
        "crimson"


jTetromino : Tetromino
jTetromino =
    JTetromino
        [ ( 1, 1 )
        , ( 0, -1 )
        , ( 1, -1 )
        , ( 1, 0 )
        ]
        "teal"


zTetromino : Tetromino
zTetromino =
    ZTetromino
        [ ( -1, -1 )
        , ( 0, -1 )
        , ( 0, 0 )
        , ( 1, 0 )
        ]
        "coral"


sTetromino : Tetromino
sTetromino =
    STetromino
        [ ( -1, -1 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( 1, -1 )
        ]
        "palevioletred"


tTetromino : Tetromino
tTetromino =
    TTetromino
        [ ( -1, 0 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( 1, -1 )
        ]
        "olivedrab"


iTetromino : Tetromino
iTetromino =
    ITetromino
        [ ( -1, -2 )
        , ( -1, -1 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
        "lightseagreen"


oTetromino : Tetromino
oTetromino =
    OTetromino
        [ ( -1, -1 )
        , ( 0, -1 )
        , ( 0, 0 )
        , ( -1, 0 )
        ]
        "goldenrod"


properties : Tetromino -> ( Shape, Color )
properties tetromino =
    case tetromino of
        OTetromino positions color ->
            ( positions, color )

        ITetromino positions color ->
            ( positions, color )

        ZTetromino positions color ->
            ( positions, color )

        STetromino positions color ->
            ( positions, color )

        TTetromino positions color ->
            ( positions, color )

        LTetromino positions color ->
            ( positions, color )

        JTetromino positions color ->
            ( positions, color )


rotateShape : Tetromino -> Tetromino
rotateShape tetromino =
    case tetromino of
        OTetromino positions color ->
            OTetromino (List.map rotateEven <| positions) color

        ITetromino positions color ->
            ITetromino (List.map rotateEven <| positions) color

        STetromino positions color ->
            ZTetromino (List.map rotateUneven <| positions) color

        ZTetromino positions color ->
            ZTetromino (List.map rotateUneven <| positions) color

        TTetromino positions color ->
            ZTetromino (List.map rotateUneven <| positions) color

        LTetromino positions color ->
            LTetromino (List.map (rotateUneven) <| positions) color

        JTetromino positions color ->
            LTetromino (List.map (rotateUneven) <| positions) color


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
