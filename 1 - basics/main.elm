module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import Tetromino exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Tile =
    ( Position, Color )


type alias Model =
    Tetromino


init : ( Model, Cmd Msg )
init =
    ( zTetromino
    , Random.generate ResetTetromino randomTetromino
    )



-- UPDATE


type Msg
    = UserInput Int
    | ResetTetromino Tetromino


update : Msg -> Model -> ( Model, Cmd Msg )
update msg tetromino =
    case msg of
        ResetTetromino newT ->
            ( newT, Cmd.none )

        UserInput 32 ->
            ( rotateShape tetromino, Cmd.none )

        _ ->
            ( tetromino, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs UserInput



-- VIEW


tileWidth =
    20


view : Model -> Html Msg
view ( positions, color ) =
    div
        []
        [ figure ( positions, color )
        ]


figure : Model -> Svg Msg
figure ( positions, color ) =
    svg []
        (List.map
            (\position -> tile ( position, color ))
            positions
        )


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
