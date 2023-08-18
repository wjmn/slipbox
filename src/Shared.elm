module Shared exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Time exposing (Posix, posixToMillis, millisToPosix)

encodePosix : Posix -> Encode.Value
encodePosix posix =
    Encode.int (posixToMillis posix)

decoderPosix : Decode.Decoder Posix
decoderPosix =
    Decode.map millisToPosix Decode.int
