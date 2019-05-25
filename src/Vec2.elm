module Vec2 exposing (Vec2, add, negate)


type alias Vec2 =
    { x : Int, y : Int }


add : Vec2 -> Vec2 -> Vec2
add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


negate : Vec2 -> Vec2
negate a =
    { x = -a.x, y = -a.y }
