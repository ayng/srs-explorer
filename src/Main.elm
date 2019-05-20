module Main exposing (main)

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


type alias Playfield =
    Matrix Bool


exampleField =
    Matrix.repeat { width = 10, height = 10 } True
        |> Matrix.stamp (&&) { x = 0, y = 0 } (Matrix.repeat { width = 4, height = 3 } False)
        |> Matrix.stamp (&&) { x = 4, y = 0 } (Matrix.repeat { width = 6, height = 5 } False)
        |> Matrix.stamp (&&) { x = 0, y = 3 } (Matrix.repeat { width = 2, height = 2 } False)
        |> Matrix.stamp (&&) { x = 2, y = 4 } (Matrix.repeat { width = 1, height = 6 } False)
        |> Matrix.stamp (&&) { x = 1, y = 6 } (Matrix.repeat { width = 1, height = 2 } False)
        |> Matrix.set { x = 3, y = 7 } False
        |> Matrix.set { x = 3, y = 7 } False
        |> Matrix.set { x = 5, y = 5 } False
        |> Matrix.set { x = 4, y = 3 } True
        |> Matrix.stamp (&&) { x = 7, y = 5 } (Matrix.repeat { width = 3, height = 2 } False)
        |> Matrix.set { x = 7, y = 5 } True
        |> Matrix.set { x = 7, y = 4 } True
        |> Matrix.set { x = 7, y = 3 } True


type alias Vec2 =
    { x : Int
    , y : Int
    }


type alias Model =
    { field : Playfield
    , piece : Tetromino.Tetromino
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { field = exampleField
      , piece = Tetromino.t
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
            ( { model | piece = Tetromino.rotateCCW model.field model.piece }, Cmd.none )

        RotateRight ->
            ( { model | piece = Tetromino.rotateCW model.field model.piece }, Cmd.none )

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


viewBoolTable : List (List Bool) -> Html msg
viewBoolTable boolTable =
    let
        rowList =
            List.map
                (\row ->
                    tr []
                        (List.map
                            (\col ->
                                td
                                    [ style "width" "36px"
                                    , style "height" "36px"
                                    , if col then
                                        style "background-color" "black"

                                      else
                                        style "background-color" "gray"
                                    ]
                                    []
                            )
                            row
                        )
                )
                boolTable
    in
    table [] rowList


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RotateLeft ] [ text "rotate left" ]
        , button [ onClick RotateRight ] [ text "rotate right" ]
        , button [ onClick (Move { x = 1, y = 0 }) ] [ text "move right" ]
        , button [ onClick (Move { x = -1, y = 0 }) ] [ text "move left" ]
        , button [ onClick (Move { x = 0, y = 1 }) ] [ text "move down" ]
        , button [ onClick (Move { x = 0, y = -1 }) ] [ text "move up" ]
        , viewBoolTable
            (Matrix.toLists
                (Matrix.stamp
                    (||)
                    model.piece.position
                    model.piece.matrix
                    model.field
                )
            )
        ]
