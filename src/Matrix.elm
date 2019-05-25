module Matrix exposing (Matrix, any, clampToEdge, dimensions, intersection, repeat, rotateCCW, rotateCW, set, stamp, toLists)

import Array exposing (Array)
import Vec2 exposing (Vec2)


type alias Dimensions =
    { width : Int, height : Int }


type Matrix a
    = Matrix Dimensions (Array a)


dimensions : Matrix a -> Dimensions
dimensions (Matrix dim _) =
    dim


toIndex : Dimensions -> Vec2 -> Maybe Int
toIndex dim coordinate =
    if inBounds dim coordinate then
        Just (dim.width * coordinate.y + coordinate.x)

    else
        Nothing


toCoordinate : Dimensions -> Int -> Maybe Vec2
toCoordinate dim index =
    if dim.width == 0 || dim.height == 0 then
        Nothing

    else
        Just { x = modBy dim.width index, y = index // dim.width }


toArray : Matrix a -> Array a
toArray (Matrix dim array) =
    array


inBounds : Dimensions -> Vec2 -> Bool
inBounds d c =
    0 <= c.x && c.x < d.width && 0 <= c.y && c.y < d.height


set : Vec2 -> a -> Matrix a -> Matrix a
set coordinate value (Matrix dim array) =
    case toIndex dim coordinate of
        Just i ->
            Matrix dim (Array.set i value array)

        Nothing ->
            Matrix dim array


get : Vec2 -> Matrix a -> Maybe a
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


indexedMap : (Vec2 -> a -> b) -> Matrix a -> Matrix b
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
repeat dim value =
    Matrix dim (Array.repeat (dim.width * dim.height) value)


toLists : Matrix a -> List (List a)
toLists (Matrix dim array) =
    List.map
        (\n ->
            Array.toList
                (Array.slice
                    (n * dim.width)
                    ((n + 1) * dim.width)
                    array
                )
        )
        (List.range 0 (dim.height - 1))


clampToEdge : Dimensions -> Vec2 -> Vec2
clampToEdge dim coordinate =
    { x = clamp 0 dim.width coordinate.x
    , y = clamp 0 dim.height coordinate.y
    }


slice : Vec2 -> Vec2 -> Matrix a -> Matrix a
slice topLeft bottomRight (Matrix dim array) =
    let
        clampedTL =
            clampToEdge dim topLeft

        clampedBR =
            clampToEdge dim bottomRight

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


stamp : (b -> a -> a) -> Vec2 -> Matrix b -> Matrix a -> Matrix a
stamp fn offset offsetMatrix fixedMatrix =
    List.foldl
        (>>)
        identity
        (indexedMap
            (\offsetCoordinate offsetValue ->
                let
                    fixedCoordinate =
                        Vec2.add offsetCoordinate offset
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


intersection : (a -> b -> c) -> Vec2 -> Matrix a -> Matrix b -> Matrix c
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
