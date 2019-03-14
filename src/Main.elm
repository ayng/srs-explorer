module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Cell
    = X
    | O


type alias Layout =
    List (List Cell)


jLayout =
    [ [ X, O, O ]
    , [ X, X, X ]
    , [ O, O, O ]
    ]


transpose : List (List a) -> List (List a)
transpose m =
    let
        maybeHead =
            List.map List.head m

        maybeTail =
            List.map List.tail m

        -- unwrap : List (Maybe a) -> Maybe (List a)
        unwrap lma =
            case lma of
                [] ->
                    Just []

                head :: tail ->
                    case head of
                        Just h ->
                            case unwrap tail of
                                Just t ->
                                    Just (h :: t)

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
    in
    case unwrap maybeHead of
        Just head ->
            head
                :: (case unwrap maybeTail of
                        Just tail ->
                            transpose tail

                        Nothing ->
                            []
                   )

        Nothing ->
            []


rotate : Bool -> List (List a) -> List (List a)
rotate clockwise grid =
    if clockwise then
        transpose (List.reverse grid)

    else
        transpose (List.map List.reverse grid)



-- MODEL


type alias Model =
    { layout : Layout }


init : Model
init =
    { layout = jLayout }



-- UPDATE


type Msg
    = RotateLeft
    | RotateRight


update : Msg -> Model -> Model
update msg model =
    case msg of
        RotateLeft ->
            { model | layout = rotate False model.layout }

        RotateRight ->
            { model | layout = rotate True model.layout }



-- VIEW


visualizeLayout : Layout -> Html msg
visualizeLayout layout =
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
                                    , style "border" "solid black 1px"
                                    , case col of
                                        X ->
                                            style "background-color" "black"

                                        O ->
                                            style "background-color" "gray"
                                    ]
                                    []
                            )
                            row
                        )
                )
                layout
    in
    table [] rowList


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick RotateLeft ] [ text "< left" ]
        , button [ onClick RotateRight ] [ text "right >" ]
        , visualizeLayout model.layout
        ]
