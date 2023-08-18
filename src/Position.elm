module Position exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode

type alias Position =
    { x : Float
    , y : Float }


setX : Position -> Float -> Position
setX { x, y } newX = { x = newX, y = y}

setY : Position -> Float -> Position
setY { x, y } newY = { x = x, y = newY}

encode : Position -> Encode.Value
encode position =
    Encode.object
        [ ("x", Encode.float position.x)
        , ("y", Encode.float position.y)]

decoder : Decode.Decoder Position
decoder =
    Decode.map2 Position
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
