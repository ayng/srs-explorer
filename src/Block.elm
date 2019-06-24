module Block exposing (Block(..), collide, isConflict, isNotEmpty)


type Block
    = Empty
    | J
    | L
    | Z
    | S
    | I
    | T
    | Garbage
    | Conflict


isConflict : Block -> Bool
isConflict block =
    case block of
        Conflict ->
            True

        _ ->
            False


isNotEmpty : Block -> Bool
isNotEmpty block =
    case block of
        Empty ->
            False

        _ ->
            True


collide : Block -> Block -> Block
collide a b =
    case a of
        Empty ->
            case b of
                Empty ->
                    Empty

                _ ->
                    b

        _ ->
            case b of
                Empty ->
                    a

                _ ->
                    Conflict
