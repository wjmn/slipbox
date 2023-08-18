module Arrangement exposing (..)

import Desk exposing (Desk)
import Time exposing (Posix)
import Shared exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

type alias Arrangement =
    { created: Posix
    , name : String
    , desk: Desk }

encode arrangement =
    Encode.object
        [ ("created", encodePosix arrangement.created)
        , ("name", Encode.string arrangement.name)
        , ("desk", Desk.encode arrangement.desk)]

decoder : Decode.Decoder Arrangement
decoder =
    Decode.map3 Arrangement
        (Decode.field "created" decoderPosix)
        (Decode.field "name" Decode.string)
        (Decode.field "desk" Desk.decoder)
