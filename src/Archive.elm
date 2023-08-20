module Archive exposing (..)

import Archived exposing (Archived)
import Card
import Desk exposing (Desk)
import Json.Decode as Decode
import Json.Encode as Encode
import Scrap exposing (Scrap)
import Slipbox exposing (Slipbox)


type alias Archive =
    { items : List Archived }


encode : Archive -> Encode.Value
encode archive =
    Encode.object
        [ ( "items", Encode.list Archived.encode archive.items ) ]


decoder =
    Decode.map Archive
        (Decode.field "items" (Decode.list Archived.decoder))


default =
    { items = [] }


withCard card now archive =
    if String.trim card.content == "" then
        archive
    else
        { archive | items = { archived = now, item = Archived.Card <| Card.toSlipbox card now } :: archive.items }

withScrap scrap now archive =
    if String.trim scrap.content == "" then
        archive
    else
        { archive | items = { archived = now, item = Archived.Scrap scrap } :: archive.items }

isEqualCard card item =
    case item.item of
        Archived.Card c ->
            c.created == card.created
        _ ->
            False

isEqualScrap scrap item =
    case item.item of
        Archived.Scrap s ->
            s.created == scrap.created
        _ ->
            False

withoutCard : Card.InSlipbox -> Archive -> Archive
withoutCard card archive =
    { archive | items = List.filter (isEqualCard card >> not) archive.items }


withoutScrap: Scrap.Scrap -> Archive -> Archive
withoutScrap scrap archive =
    { archive | items = List.filter (isEqualScrap scrap >> not) archive.items }
