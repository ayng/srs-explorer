module Tetromino exposing (Tetromino, i, j, l, move, rotateCCW, rotateCW, s, t, z)

import List.Extra
import Matrix exposing (Matrix)
import Vec2


type alias Vec2 =
    { x : Int
    , y : Int
    }


type alias Playfield =
    Matrix Bool


type Kind
    = J
    | L
    | S
    | T
    | Z
    | I


type alias Tetromino =
    { matrix : Matrix Bool
    , orientation : Orientation
    , position : Vec2
    , kind : Kind
    }


type Rotation
    = Clockwise
    | Counterclockwise


type Orientation
    = Up
    | Down
    | Left
    | Right


j : Tetromino
j =
    { matrix =
        Matrix.repeat { width = 3, height = 3 } False
            |> Matrix.set { x = 0, y = 0 } True
            |> Matrix.set { x = 0, y = 1 } True
            |> Matrix.set { x = 1, y = 1 } True
            |> Matrix.set { x = 2, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = J
    }


l : Tetromino
l =
    { matrix =
        Matrix.repeat { width = 3, height = 3 } False
            |> Matrix.set { x = 2, y = 0 } True
            |> Matrix.set { x = 0, y = 1 } True
            |> Matrix.set { x = 1, y = 1 } True
            |> Matrix.set { x = 2, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = L
    }


s : Tetromino
s =
    { matrix =
        Matrix.repeat { width = 3, height = 3 } False
            |> Matrix.set { x = 1, y = 0 } True
            |> Matrix.set { x = 2, y = 0 } True
            |> Matrix.set { x = 0, y = 1 } True
            |> Matrix.set { x = 1, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = S
    }


t : Tetromino
t =
    { matrix =
        Matrix.repeat { width = 3, height = 3 } False
            |> Matrix.set { x = 1, y = 0 } True
            |> Matrix.set { x = 0, y = 1 } True
            |> Matrix.set { x = 1, y = 1 } True
            |> Matrix.set { x = 2, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = T
    }


z : Tetromino
z =
    { matrix =
        Matrix.repeat { width = 3, height = 3 } False
            |> Matrix.set { x = 0, y = 0 } True
            |> Matrix.set { x = 1, y = 0 } True
            |> Matrix.set { x = 1, y = 1 } True
            |> Matrix.set { x = 2, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = Z
    }


i : Tetromino
i =
    { matrix =
        Matrix.repeat { width = 4, height = 4 } False
            |> Matrix.set { x = 0, y = 1 } True
            |> Matrix.set { x = 1, y = 1 } True
            |> Matrix.set { x = 2, y = 1 } True
            |> Matrix.set { x = 3, y = 1 } True
    , position = { x = 0, y = 0 }
    , orientation = Up
    , kind = I
    }


orient : Rotation -> Orientation -> Orientation
orient rotation orientation =
    case orientation of
        Up ->
            case rotation of
                Clockwise ->
                    Right

                Counterclockwise ->
                    Left

        Right ->
            case rotation of
                Clockwise ->
                    Down

                Counterclockwise ->
                    Up

        Down ->
            case rotation of
                Clockwise ->
                    Left

                Counterclockwise ->
                    Right

        Left ->
            case rotation of
                Clockwise ->
                    Up

                Counterclockwise ->
                    Down


iOffsets : Rotation -> Orientation -> List Vec2
iOffsets rotation orientation =
    case rotation of
        Clockwise ->
            case orientation of
                Up ->
                    [ { x = 0, y = 0 }
                    , { x = -2, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = -2, y = 1 }
                    , { x = 1, y = -2 }
                    ]

                Right ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = 2, y = 0 }
                    , { x = -1, y = -2 }
                    , { x = 2, y = 1 }
                    ]

                Down ->
                    [ { x = 0, y = 0 }
                    , { x = 2, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = 2, y = -1 }
                    , { x = -1, y = 2 }
                    ]

                Left ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = -2, y = 0 }
                    , { x = 1, y = 2 }
                    , { x = -2, y = -1 }
                    ]

        Counterclockwise ->
            case orientation of
                Up ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = 2, y = 0 }
                    , { x = -1, y = -2 }
                    , { x = 2, y = 1 }
                    ]

                Right ->
                    [ { x = 0, y = 0 }
                    , { x = 2, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = 2, y = -1 }
                    , { x = -1, y = 2 }
                    ]

                Down ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = -2, y = 0 }
                    , { x = 1, y = 2 }
                    , { x = -2, y = -1 }
                    ]

                Left ->
                    [ { x = 0, y = 0 }
                    , { x = -2, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = -2, y = 1 }
                    , { x = 1, y = -2 }
                    ]


jlstzOffsets : Rotation -> Orientation -> List Vec2
jlstzOffsets rotation orientation =
    case rotation of
        Clockwise ->
            case orientation of
                Up ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = -1, y = -1 }
                    , { x = 0, y = 2 }
                    , { x = -1, y = 2 }
                    ]

                Right ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = 1, y = 1 }
                    , { x = 0, y = -2 }
                    , { x = 1, y = -2 }
                    ]

                Down ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = 1, y = -1 }
                    , { x = 0, y = 2 }
                    , { x = 1, y = 2 }
                    ]

                Left ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = -1, y = 1 }
                    , { x = 0, y = -2 }
                    , { x = -1, y = -2 }
                    ]

        Counterclockwise ->
            case orientation of
                Up ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = 1, y = -1 }
                    , { x = 0, y = 2 }
                    , { x = 1, y = 2 }
                    ]

                Right ->
                    [ { x = 0, y = 0 }
                    , { x = 1, y = 0 }
                    , { x = 1, y = 1 }
                    , { x = 0, y = -2 }
                    , { x = 1, y = -2 }
                    ]

                Down ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = -1, y = -1 }
                    , { x = 0, y = 2 }
                    , { x = -1, y = 2 }
                    ]

                Left ->
                    [ { x = 0, y = 0 }
                    , { x = -1, y = 0 }
                    , { x = -1, y = 1 }
                    , { x = 0, y = -2 }
                    , { x = -1, y = -2 }
                    ]


rotateCW =
    rotate Clockwise


rotateCCW =
    rotate Counterclockwise


rotate : Rotation -> Playfield -> Tetromino -> Tetromino
rotate rotation playfield tetromino =
    let
        getOffsets =
            case tetromino.kind of
                I ->
                    iOffsets

                _ ->
                    jlstzOffsets

        offsets =
            getOffsets rotation tetromino.orientation

        newOrientation =
            orient rotation tetromino.orientation

        rotatedMatrix =
            case rotation of
                Clockwise ->
                    Matrix.rotateCW tetromino.matrix

                Counterclockwise ->
                    Matrix.rotateCCW tetromino.matrix

        workingOffset =
            List.Extra.find
                (\offset ->
                    not
                        (Matrix.doesCollide
                            (Vec2.add tetromino.position offset)
                            rotatedMatrix
                            playfield
                        )
                )
                offsets

        newTetromino =
            case workingOffset of
                Just o ->
                    { tetromino
                        | orientation = newOrientation
                        , matrix = rotatedMatrix
                        , position = Vec2.add tetromino.position o
                    }

                Nothing ->
                    tetromino
    in
    newTetromino


move : Vec2 -> Playfield -> Tetromino -> Tetromino
move offset playfield tetromino =
    let
        newPosition =
            Vec2.add offset tetromino.position
    in
    if Matrix.doesCollide newPosition tetromino.matrix playfield then
        tetromino

    else
        { tetromino | position = newPosition }
