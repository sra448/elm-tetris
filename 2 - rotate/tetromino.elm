module Tetromino exposing (..)

import List exposing (..)


-- MODEL


type alias Position =
    ( Int, Int )


type alias Color =
    String


type alias Tetromino =
    ( List Position, Color )


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



-- Rotating


rotateShape : Tetromino -> Tetromino
rotateShape ( positions, color ) =
    let
        tilesWidth =
            tetrominoWidth ( positions, color )

        rotate =
            if tilesWidth + 1 == 3 then
                rotate3
            else
                rotate4
    in
        if tilesWidth + 1 == 2 then
            ( positions, color )
        else
            ( List.map rotate positions, color )


tetrominoWidth : Tetromino -> Int
tetrominoWidth ( positions, _ ) =
    foldl max 0 <|
        List.map (\( x, y ) -> max x y) positions


rotate4 : Position -> Position
rotate4 position =
    case position of
        ( 0, 3 ) ->
            ( 0, 0 )

        ( 0, 2 ) ->
            ( 1, 0 )

        ( 0, 1 ) ->
            ( 2, 0 )

        ( 0, 0 ) ->
            ( 3, 0 )

        ( 1, 3 ) ->
            ( 0, 1 )

        ( 1, 2 ) ->
            ( 1, 1 )

        ( 1, 1 ) ->
            ( 2, 1 )

        ( 1, 0 ) ->
            ( 3, 1 )

        ( 2, 3 ) ->
            ( 0, 2 )

        ( 2, 2 ) ->
            ( 1, 2 )

        ( 2, 1 ) ->
            ( 2, 2 )

        ( 2, 0 ) ->
            ( 3, 2 )

        ( 3, 3 ) ->
            ( 0, 3 )

        ( 3, 2 ) ->
            ( 1, 3 )

        ( 3, 1 ) ->
            ( 2, 3 )

        ( 3, 0 ) ->
            ( 3, 3 )

        other ->
            other


rotate3 : Position -> Position
rotate3 position =
    case position of
        ( 0, 2 ) ->
            ( 0, 0 )

        ( 0, 1 ) ->
            ( 1, 0 )

        ( 0, 0 ) ->
            ( 2, 0 )

        ( 1, 2 ) ->
            ( 0, 1 )

        ( 1, 1 ) ->
            ( 1, 1 )

        ( 1, 0 ) ->
            ( 2, 1 )

        ( 2, 2 ) ->
            ( 0, 2 )

        ( 2, 1 ) ->
            ( 1, 2 )

        ( 2, 0 ) ->
            ( 2, 2 )

        other ->
            other
