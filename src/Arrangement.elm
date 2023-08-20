module Arrangement exposing (..)

import Desk exposing (Desk)
import Time exposing (Posix)
import Shared exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Position exposing (Position)


type alias CardMeta =
    { created : Posix
    , position : Position
    , zIndex : Float
    }


type alias Arrangement =
    { name : String
    , cards: List CardMeta}

encodeCardMeta card =
    Encode.object
        [("created", Shared.encodePosix card.created)
        , ("position", Position.encode card.position)
        , ("zIndex", Encode.float card.zIndex)]

decoderCardMeta =
    Decode.map3 CardMeta
        (Decode.field "created" Shared.decoderPosix)
        (Decode.field "position" Position.decoder )
        (Decode.field "zIndex" Decode.float)

encode arrangement =
    Encode.object
        [("name", Encode.string arrangement.name)
        , ("cards", Encode.list encodeCardMeta arrangement.cards)]

decoder : Decode.Decoder Arrangement
decoder =
    Decode.map2 Arrangement
        (Decode.field "name" Decode.string)
        (Decode.field "cards" (Decode.list decoderCardMeta))


fromDesk : String -> Desk -> Arrangement
fromDesk name desk =
    let
        cardToMeta c =
            { created =  c.created
            , position = c.position
            , zIndex = c.zIndex}
    in
        { name = name
        , cards = desk.cards |> List.map cardToMeta }
