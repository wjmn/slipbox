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

withNewCard card desk =
    let
        maxZ =
            List.map .zIndex desk.cards
            |> List.maximum
            |> Maybe.withDefault 0
    in
    { desk | cards = (Card.withZIndex (maxZ + 1) card) :: desk.cards }

withoutCard card desk =
    let
        calibrateZ oldCard =
            if oldCard.zIndex > card.zIndex then
                { oldCard | zIndex = oldCard.zIndex - 1 }
            else
                oldCard
    in
    { desk | cards =
          List.filter (\c -> c.created /= card.created) desk.cards |> List.map calibrateZ}

modifyCard posix f desk =
    let
        fif c =
            if posix == c.created then
                f c
            else
                c
    in
    { desk | cards = List.map fif desk.cards }
