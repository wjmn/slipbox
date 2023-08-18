module Desk exposing (..)

import Card
import Json.Encode as Encode
import Json.Decode as Decode

type alias Desk =
    { cards : List Card.OnDesk }

encode desk =
    Encode.object
        [("cards", Encode.list Card.encodeOnDesk desk.cards)]

decoder =
    Decode.map Desk
        (Decode.field "cards" (Decode.list Card.decoderOnDesk))

default =
    { cards = [] }
