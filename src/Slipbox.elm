module Slipbox exposing (..)

import Arrangement exposing (Arrangement)
import Card
import Colour exposing (Colour)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)


type SortOrder
    = SortByCreationDateRecentFirst
    | SortByCreationDateRecentLast
    | SortByModifiedDateRecentFirst
    | SortByModifiedDateRecentLast
    | SortByColour
    | SortByColourReversed


type alias Slipbox =
    { cards : List Card.InSlipbox
    , sortOrder : SortOrder
    , searchString : String
    , colourFilter : Set Int
    , arrangements : List Arrangement
    }


sortOrderToInt order =
    case order of
        SortByCreationDateRecentFirst ->
            0

        SortByCreationDateRecentLast ->
            1

        SortByModifiedDateRecentFirst ->
            2

        SortByModifiedDateRecentLast ->
            3

        SortByColour ->
            4

        SortByColourReversed ->
            5


intToSortOrder int =
    case int of
        0 ->
            SortByCreationDateRecentFirst

        1 ->
            SortByCreationDateRecentLast

        2 ->
            SortByModifiedDateRecentFirst

        3 ->
            SortByModifiedDateRecentLast

        4 ->
            SortByColour

        5 ->
            SortByColourReversed

        _ ->
            SortByCreationDateRecentFirst


encode slipbox =
    Encode.object
        [ ( "cards", Encode.list Card.encodeInSlipbox slipbox.cards )
        , ( "sortOrder", Encode.int (sortOrderToInt slipbox.sortOrder) )
        , ( "searchString", Encode.string slipbox.searchString )
        , ( "colourFilter", Encode.list Encode.int (Set.toList slipbox.colourFilter) )
        , ( "arrangements", Encode.list Arrangement.encode slipbox.arrangements )
        ]


decoder =
    Decode.map5 Slipbox
        (Decode.field "cards" (Decode.list Card.decoderInSlipbox))
        (Decode.field "sortOrder" (Decode.map intToSortOrder Decode.int))
        (Decode.field "searchString" Decode.string)
        (Decode.field "colourFilter" (Decode.map Set.fromList (Decode.list Decode.int)))
        (Decode.field "arrangements" (Decode.list Arrangement.decoder))

default =
    { cards = []
    , sortOrder = SortByCreationDateRecentFirst
    , searchString = ""
    , colourFilter = Set.empty
    , arrangements =  []
    }

withCard card now slipbox =
    { slipbox | cards = (Card.toSlipbox card now) :: slipbox.cards }

withoutCard card slipbox =
    { slipbox | cards =
          List.filter (\c -> c.created /= card.created) slipbox.cards }
