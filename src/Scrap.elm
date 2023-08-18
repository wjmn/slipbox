module Scrap exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Shared exposing (..)
import Time exposing (Posix)


type alias Scrap =
    { created : Posix
    , content : String
    }


encode scrap =
    Encode.object
        [ ( "created", encodePosix scrap.created )
        , ( "content", Encode.string scrap.content )
        ]


decoder =
    Decode.map2 Scrap
        (Decode.field "created" decoderPosix)
        (Decode.field "content" Decode.string)

default timestamp =
    { created = timestamp
    , content = ""}


withContent content scrap =
    { scrap | content = content }
