module Matrix exposing (Matrix, doesCollide, repeat, rotateCCW, rotateCW, set, stamp, toLists)

import Array exposing (Array)


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


rotateCW : Matrix a -> Matrix a
rotateCW =
    rotate True


rotateCCW : Matrix a -> Matrix a
rotateCCW =
    rotate False


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
