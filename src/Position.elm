module Position exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias Position =
    { x : Float
    , y : Float
    }


setX : Position -> Float -> Position
setX { x, y } newX =
    { x = newX, y = y }


setY : Position -> Float -> Position
setY { x, y } newY =
    { x = x, y = newY }


encode : Position -> Encode.Value
encode position =
    Encode.object
        [ ( "x", Encode.float position.x )
        , ( "y", Encode.float position.y )
        ]


decoder : Decode.Decoder Position
decoder =
    Decode.map2 Position
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)


minus a b =
    { x = a.x - b.x, y = a.y - b.y }
