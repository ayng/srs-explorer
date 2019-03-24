module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Debug
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Dimensions =
    { width : Int, height : Int }


type alias Coordinate =
    { x : Int, y : Int }


type Matrix a
    = Matrix Dimensions (Array a)


toIndex : Dimensions -> Coordinate -> Maybe Int
toIndex dimensions coordinate =
    if inBounds dimensions coordinate then
        Just (dimensions.width * coordinate.y + coordinate.x)

    else
        Nothing


toCoordinate : Dimensions -> Int -> Maybe Coordinate
toCoordinate dimensions index =
    if dimensions.width == 0 || dimensions.height == 0 then
        Nothing

    else
        Just { x = modBy dimensions.width index, y = index // dimensions.width }


toArray : Matrix a -> Array a
toArray (Matrix dim array) =
    array


inBounds : Dimensions -> Coordinate -> Bool
inBounds d c =
    0 <= c.x && c.x < d.width && 0 <= c.y && c.y < d.height


add : Coordinate -> Coordinate -> Coordinate
add a b =
    { x = a.x + b.x, y = a.y + b.y }


negate : Coordinate -> Coordinate
negate a =
    { x = -a.x, y = -a.y }


set : Coordinate -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dimensions array) =
    case toIndex dimensions coordinate of
        Just i ->
            Matrix dimensions (Array.set i value array)

        Nothing ->
            Matrix dimensions array


get : Coordinate -> Matrix a -> Maybe a
get coordinate (Matrix dim array) =
    case toIndex dim coordinate of
        Just i ->
            case Array.get i array of
                Just v ->
                    Just v

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


indexedMap : (Coordinate -> a -> b) -> Matrix a -> Matrix b
indexedMap fn (Matrix dim array) =
    Array.indexedMap
        (\n v ->
            case toCoordinate dim n of
                Just c ->
                    fn c v

                -- FIXME this should never happen
                Nothing ->
                    Debug.todo "index from matrix array was out of bounds of matrix"
        )
        array
        |> Matrix dim


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


coordinateClamp : Dimensions -> Coordinate -> Coordinate
coordinateClamp dim coordinate =
    { x = clamp 0 dim.width coordinate.x
    , y = clamp 0 dim.height coordinate.y
    }


slice : Coordinate -> Coordinate -> Matrix a -> Matrix a
slice topLeft bottomRight (Matrix dim array) =
    let
        clampedTL =
            coordinateClamp dim topLeft

        clampedBR =
            coordinateClamp dim bottomRight

        newDim =
            { width = clampedBR.x - clampedTL.x
            , height = clampedBR.y - clampedTL.y
            }
    in
    if newDim.width * newDim.height == 0 then
        Matrix newDim Array.empty

    else
        List.map
            (\n ->
                case toIndex dim { clampedTL | y = clampedTL.y + n } of
                    Just i ->
                        Array.slice i (i + newDim.width) array

                    -- FIXME this should never happen
                    Nothing ->
                        Debug.todo "coordinate was outside dimensions in which it was clamped"
            )
            (List.range 0 (newDim.height - 1))
            |> List.foldr Array.append Array.empty
            |> Matrix newDim


any : (a -> Bool) -> Matrix a -> Bool
any check (Matrix _ array) =
    Array.map check array
        |> Array.foldl (||) False


doesCollide : Coordinate -> Matrix Bool -> Matrix Bool -> Bool
doesCollide offset piece field =
    let
        (Matrix pieceDim _) =
            piece

        (Matrix insideDim _) =
            insidePart

        insidePart =
            intersection (&&) offset piece field

        outsidePart =
            stamp
                (\_ _ -> False)
                (coordinateClamp pieceDim (negate offset))
                (repeat insideDim False)
                piece

        doesCollideWithWall =
            any identity outsidePart

        doesCollideInside =
            any identity insidePart
    in
    doesCollideWithWall || doesCollideInside


stamp : (b -> a -> a) -> Coordinate -> Matrix b -> Matrix a -> Matrix a
stamp fn offset offsetMatrix fixedMatrix =
    List.foldl
        (>>)
        identity
        (indexedMap
            (\offsetCoordinate offsetValue ->
                let
                    fixedCoordinate =
                        add offsetCoordinate offset
                in
                case get fixedCoordinate fixedMatrix of
                    Just fixedValue ->
                        set fixedCoordinate (fn offsetValue fixedValue)

                    Nothing ->
                        identity
            )
            offsetMatrix
            |> toArray
            |> Array.toList
        )
        fixedMatrix


intersection : (a -> b -> c) -> Coordinate -> Matrix a -> Matrix b -> Matrix c
intersection fn offset (Matrix offsetDim offsetArray) (Matrix fixedDim fixedArray) =
    let
        offsetTL =
            offset

        offsetBR =
            { x = offset.x + offsetDim.width
            , y = offset.y + offsetDim.height
            }

        (Matrix trimmedFixedDim trimmedFixedArray) =
            slice offsetTL offsetBR (Matrix fixedDim fixedArray)

        fixedTL =
            { x = -offset.x
            , y = -offset.y
            }

        fixedBR =
            { x = -offset.x + fixedDim.width
            , y = -offset.y + fixedDim.height
            }

        (Matrix trimmedOffsetDim trimmedOffsetArray) =
            slice fixedTL fixedBR (Matrix offsetDim offsetArray)
    in
    List.map2
        fn
        (Array.toList trimmedOffsetArray)
        (Array.toList trimmedFixedArray)
        |> Array.fromList
        |> Matrix trimmedOffsetDim


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


jMatrix =
    repeat { width = 3, height = 3 } False
        |> set { x = 0, y = 0 } True
        |> set { x = 0, y = 1 } True
        |> set { x = 1, y = 1 } True
        |> set { x = 2, y = 1 } True


exampleField =
    repeat { width = 5, height = 5 } False
        |> set { x = 0, y = 2 } True
        |> set { x = 0, y = 3 } True
        |> set { x = 3, y = 3 } True
        |> set { x = 4, y = 3 } True
        |> set { x = 0, y = 4 } True
        |> set { x = 4, y = 4 } True


type alias Model =
    { field : Matrix Bool
    , piece : Matrix Bool
    , offset : Coordinate
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { field = exampleField
      , piece = jMatrix
      , offset = { x = 0, y = 0 }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = RotateLeft
    | RotateRight
    | Move Coordinate
    | Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RotateLeft ->
            ( { model | piece = rotate False model.piece }, Cmd.none )

        RotateRight ->
            ( { model | piece = rotate True model.piece }, Cmd.none )

        Move c ->
            ( { model
                | offset =
                    { x = model.offset.x + c.x
                    , y = model.offset.y + c.y
                    }
              }
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

        --, viewBoolTable (toLists model.field)
        , viewBoolTable
            (toLists
                (stamp
                    (||)
                    model.offset
                    model.piece
                    model.field
                )
            )
        , viewBoolTable (toLists (intersection (||) model.offset model.piece model.field))

        --, viewBoolTable (toLists model.piece)
        --, viewBoolTable (toLists (slice { x = 1, y = 1 } { x = 3, y = 3 } jMatrix))
        --, viewBoolTable (toLists (intersection (&&) model.offset jMatrix jMatrix))
        , text "is colliding: "
        , text (boolToString (doesCollide model.offset model.piece model.field))

        --, text (boolToString (doesCollide jMatrix jMatrix { x = 1, y = -1 }))
        ]
