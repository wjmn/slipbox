module Card exposing (..)

import Colour exposing (Colour)
import Position exposing (Position)
import Time exposing (Posix)
import Json.Encode as Encode
import Json.Decode as Decode
import Shared exposing (..)


-- | Creation timestamp (in ms) is used as a unique ID.
-- I do not think the risk of collisions is significant when this application is used by a single person as intended.
type alias InSlipbox =
    { created : Posix
    , modified : Posix
    , content : String
    , colour : Colour
    }


type alias OnDesk =
    { created : Posix
    , previousModified : Posix
    , content : String
    , colour : Colour
    , position : Position
    , zIndex : Float
    }

encodeInSlipbox : InSlipbox -> Encode.Value
encodeInSlipbox card =
    Encode.object
        [ ("created", encodePosix card.created)
        , ("modified", encodePosix card.modified)
        , ("content", Encode.string card.content)
        , ("colour", Colour.encode card.colour)
        ]

encodeOnDesk card =
    Encode.object
        [ ("created", encodePosix card.created)
        , ("previousModified", encodePosix card.previousModified)
        , ("content", Encode.string card.content)
        , ("colour", Colour.encode card.colour)
        , ("position", Position.encode card.position)
        , ("zIndex", Encode.float card.zIndex)
        ]

decoderInSlipbox : Decode.Decoder InSlipbox
decoderInSlipbox =
    Decode.map4 InSlipbox
        (Decode.field "created" decoderPosix)
        (Decode.field "modified" decoderPosix)
        (Decode.field "content" Decode.string)
        (Decode.field "colour" Colour.decoder)

decoderOnDesk : Decode.Decoder OnDesk
decoderOnDesk =
    Decode.map6 OnDesk
        (Decode.field "created" decoderPosix)
        (Decode.field "previousModified" decoderPosix)
        (Decode.field "content" Decode.string)
        (Decode.field "colour" Colour.decoder)
        (Decode.field "position" Position.decoder)
        (Decode.field "zIndex" Decode.float)
