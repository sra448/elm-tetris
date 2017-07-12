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
