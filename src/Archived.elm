module Archived exposing (..)

import Card
import Scrap exposing (Scrap)
import Time exposing (Posix)
import Shared exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

type Item
    = Card Card.InSlipbox
    | Scrap Scrap

itemToInt : Item -> Int
itemToInt item =
    case item of
        Card _ -> 0
        Scrap _ -> 1

type alias Archived =
    { archived: Posix
    , item: Item }

encodeItem item =
    case item of
        Card card->
            Encode.object
                [("type", Encode.int (itemToInt item))
                , ("card", Card.encodeInSlipbox card)]
        Scrap scrap ->
            Encode.object
                [("type", Encode.int (itemToInt item))
                , ("scrap", Scrap.encode scrap)]

encode archived =
    Encode.object
        [("archived", encodePosix archived.archived)
        , ("item", encodeItem archived.item)]

decoderItem =
    Decode.oneOf
        [ Decode.map Card (Decode.field "card" Card.decoderInSlipbox)
        , Decode.map Scrap (Decode.field "scrap" Scrap.decoder)]

decoder =
    Decode.map2 Archived
        (Decode.field "archived" decoderPosix)
        (Decode.field "item" decoderItem)
