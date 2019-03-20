module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Debug
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Dimensions =
    { width : Int, height : Int }


type alias Coordinate =
    { x : Int, y : Int }


type Matrix a
    = Matrix Dimensions (Array a)


toIndex : Dimensions -> Coordinate -> Int
toIndex dimensions coordinate =
    dimensions.width * coordinate.y + coordinate.x


set : Coordinate -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dimensions array) =
    Matrix dimensions (Array.set (toIndex dimensions coordinate) value array)


repeat : Dimensions -> a -> Matrix a
repeat dimensions value =
    Matrix dimensions (Array.repeat (dimensions.width * dimensions.height) value)


toLists : Matrix a -> List (List a)
toLists (Matrix dimensions array) =
    List.map
        (\n ->
            Array.toList
                (Array.slice
                    (n * dimensions.width)
                    ((n + 1) * dimensions.width)
                    array
                )
        )
        (List.range 0 (dimensions.height - 1))


jMatrix =
    repeat { width = 3, height = 3 } False
        |> set { x = 0, y = 0 } True
        |> set { x = 0, y = 1 } True
        |> set { x = 1, y = 1 } True
        |> set { x = 2, y = 1 } True



{-
   exampleField =
       [ [ O, O, O, O, O ]
       , [ O, O, O, O, O ]
       , [ X, O, O, X, X ]
       , [ X, O, O, X, X ]
       , [ X, O, O, O, X ]
       ]
-}


slice : Coordinate -> Coordinate -> Matrix a -> Matrix a
slice topLeft bottomRight (Matrix dim array) =
    let
        newDim =
            { width = bottomRight.x - topLeft.x
            , height = bottomRight.y - topLeft.y
            }
    in
    List.map
        (\n ->
            let
                rowStart =
                    toIndex dim { topLeft | y = topLeft.y + n }

                rowEnd =
                    rowStart + newDim.width
            in
            Array.slice rowStart rowEnd array
        )
        (List.range 0 (newDim.height - 1))
        |> List.foldr Array.append Array.empty
        |> Matrix newDim


doesCollide : Matrix Bool -> Matrix Bool -> Coordinate -> Bool
doesCollide (Matrix fieldDim fieldArray) (Matrix pieceDim pieceArray) offset =
    let
        pieceTopLeft =
            { x = clamp 0 pieceDim.width -offset.x
            , y = clamp 0 pieceDim.height -offset.y
            }

        pieceBottomRight =
            { x = clamp 0 pieceDim.width (fieldDim.width - offset.x)
            , y = clamp 0 pieceDim.height (fieldDim.height - offset.y)
            }

        -- TODO address any cells that get trimmed off. these are "out of bounds"
        (Matrix trimmedPieceDim trimmedPieceArray) =
            slice pieceTopLeft pieceBottomRight (Matrix pieceDim pieceArray)

        fieldTopLeft =
            { x = clamp 0 fieldDim.width offset.x
            , y = clamp 0 fieldDim.height offset.y
            }

        fieldBottomRight =
            { x = fieldTopLeft.x + trimmedPieceDim.width
            , y = fieldTopLeft.y + trimmedPieceDim.height
            }

        (Matrix trimmedFieldDim trimmedFieldArray) =
            slice fieldTopLeft fieldBottomRight (Matrix fieldDim fieldArray)
    in
    List.map2
        (\x y -> x && y)
        (Array.toList trimmedFieldArray)
        (Array.toList trimmedPieceArray)
        |> List.foldl (||) False


rotate : Bool -> Matrix a -> Matrix a
rotate cw (Matrix dim array) =
    let
        lists =
            toLists (Matrix dim array)

        rotated =
            rotateLists cw lists

        flattened =
            List.foldr List.append [] rotated

        newArray =
            Array.fromList flattened
    in
    Matrix { width = dim.height, height = dim.width } newArray


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


rotateLists : Bool -> List (List a) -> List (List a)
rotateLists cw grid =
    if cw then
        transpose (List.reverse grid)

    else
        transpose (List.map List.reverse grid)



-- MODEL


type alias Model =
    { piece : Matrix Bool }


init : Model
init =
    { piece = jMatrix }



-- UPDATE


type Msg
    = RotateLeft
    | RotateRight


update : Msg -> Model -> Model
update msg model =
    case msg of
        RotateLeft ->
            { model | piece = rotate False model.piece }

        RotateRight ->
            { model | piece = rotate True model.piece }



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
        [ button [ onClick RotateLeft ] [ text "< left" ]
        , button [ onClick RotateRight ] [ text "right >" ]
        , viewBoolTable (toLists model.piece)

        --, viewBoolTable (toLists (slice { x = 0, y = 0 } { x = 3, y = 3 } jMatrix))
        --, text (boolToString (doesCollide jMatrix jMatrix { x = 1, y = -1 }))
        ]
