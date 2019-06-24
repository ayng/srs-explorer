module Main exposing (main)

import Block exposing (Block)
import Browser
import Browser.Events
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Matrix exposing (Matrix)
import Tetromino
import Vec2 exposing (Vec2)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


exampleField =
    Matrix.repeat { width = 10, height = 10 } Block.Garbage
        |> Matrix.stamp (\a _ -> a) { x = 0, y = 0 } (Matrix.repeat { width = 4, height = 3 } Block.Empty)
        |> Matrix.stamp (\a _ -> a) { x = 4, y = 0 } (Matrix.repeat { width = 6, height = 5 } Block.Empty)
        |> Matrix.stamp (\a _ -> a) { x = 0, y = 3 } (Matrix.repeat { width = 2, height = 2 } Block.Empty)
        |> Matrix.stamp (\a _ -> a) { x = 2, y = 4 } (Matrix.repeat { width = 1, height = 6 } Block.Empty)
        |> Matrix.stamp (\a _ -> a) { x = 1, y = 6 } (Matrix.repeat { width = 1, height = 2 } Block.Empty)
        |> Matrix.set { x = 3, y = 7 } Block.Empty
        |> Matrix.set { x = 3, y = 7 } Block.Empty
        |> Matrix.set { x = 5, y = 5 } Block.Empty
        |> Matrix.set { x = 4, y = 3 } Block.Garbage
        |> Matrix.stamp (\a _ -> a) { x = 7, y = 5 } (Matrix.repeat { width = 3, height = 2 } Block.Empty)
        |> Matrix.set { x = 7, y = 5 } Block.Garbage
        |> Matrix.set { x = 7, y = 4 } Block.Garbage
        |> Matrix.set { x = 7, y = 3 } Block.Garbage


type alias Vec2 =
    { x : Int
    , y : Int
    }


type alias Model =
    { field : Tetromino.Playfield
    , piece : Tetromino.Tetromino
    , scenarios : List Tetromino.Playfield
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { field = exampleField
      , piece = Tetromino.t
      , scenarios = [ exampleField, exampleField ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = RotateLeft
    | RotateRight
    | Move Vec2
    | Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RotateLeft ->
            ( { model
                | piece = Tetromino.rotateCCW model.field model.piece
                , scenarios = Tetromino.rotateScenariosCCW model.field model.piece
              }
            , Cmd.none
            )

        RotateRight ->
            ( { model
                | piece = Tetromino.rotateCW model.field model.piece
                , scenarios = Tetromino.rotateScenariosCW model.field model.piece
              }
            , Cmd.none
            )

        Move c ->
            ( { model | piece = Tetromino.move c model.field model.piece }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\keyString ->
            case keyString of
                "ArrowUp" ->
                    Move { x = 0, y = -1 }

                "ArrowDown" ->
                    Move { x = 0, y = 1 }

                "ArrowLeft" ->
                    Move { x = -1, y = 0 }

                "ArrowRight" ->
                    Move { x = 1, y = 0 }

                "z" ->
                    RotateLeft

                "x" ->
                    RotateRight

                _ ->
                    Other
        )
        (Decode.field "key" Decode.string)



-- VIEW


multiplicationSignString =
    String.fromChar (Char.fromCode 215)


viewBlock : Int -> Block -> Html msg
viewBlock size block =
    let
        attributes =
            [ style "width" (String.append (String.fromInt size) "px")
            , style "height" (String.append (String.fromInt size) "px")
            ]
    in
    case block of
        Block.Empty ->
            td (List.append attributes [ style "background-color" "lightgray" ]) []

        Block.Conflict ->
            td
                (List.append attributes
                    [ style "color" "red"
                    , style "background-color" "pink"
                    , style "font-size" (String.append (String.fromInt size) "px")
                    , style "text-align" "center"
                    , style "line-height" "0"
                    ]
                )
                [ text multiplicationSignString ]

        Block.Garbage ->
            td (List.append attributes [ style "background-color" "gray" ]) []

        _ ->
            td (List.append attributes [ style "background-color" "black" ]) []


viewBlockTable : Int -> List (List Block) -> Html msg
viewBlockTable size blockTable =
    let
        rowList =
            List.map
                (\row -> tr [] (List.map (viewBlock size) row))
                blockTable
    in
    table [ style "display" "inline-block" ] rowList


viewPlayfield : Int -> Tetromino.Playfield -> Html msg
viewPlayfield size playfield =
    viewBlockTable size (Matrix.toLists playfield)


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick RotateLeft ] [ text "rotate left" ]
            , button [ onClick RotateRight ] [ text "rotate right" ]
            , button [ onClick (Move { x = 1, y = 0 }) ] [ text "move right" ]
            , button [ onClick (Move { x = -1, y = 0 }) ] [ text "move left" ]
            , button [ onClick (Move { x = 0, y = 1 }) ] [ text "move down" ]
            , button [ onClick (Move { x = 0, y = -1 }) ] [ text "move up" ]
            ]
        , viewBlockTable
            32
            (Matrix.toLists
                (Matrix.stamp
                    Block.collide
                    model.piece.position
                    model.piece.matrix
                    model.field
                )
            )
        , div []
            (List.map
                (\pf ->
                    div
                        [ style "border" "1px solid black"
                        , style "display" "inline-block"
                        , style "margin" "8px"
                        , style "padding" "8px"
                        ]
                        [ viewPlayfield 16 pf ]
                )
                model.scenarios
            )
        ]
