module Workspace exposing (..)

import Archive exposing (Archive)
import Desk exposing (Desk)
import Json.Decode as Decode
import Json.Encode as Encode
import Scrap exposing (Scrap)
import Slipbox exposing (Slipbox)
import Arrangement exposing (Arrangement)
import Set
import Card
import Time exposing (millisToPosix)
import Time exposing (posixToMillis)


type alias Workspace =
    { desk : Desk
    , slipbox : Slipbox
    , scrap : Scrap
    , archive : Archive
    , arrangements : List Arrangement
    , modified : Bool
    }


encode workspace =
    Encode.object
        [ ( "desk", Desk.encode workspace.desk )
        , ( "slipbox", Slipbox.encode workspace.slipbox )
        , ( "scrap", Scrap.encode workspace.scrap )
        , ( "archive", Archive.encode workspace.archive)
        , ("arrangements", Encode.list Arrangement.encode workspace.arrangements)
        ]


decoder =
    Decode.map6 Workspace
        (Decode.field "desk" Desk.decoder)
        (Decode.field "slipbox" Slipbox.decoder)
        (Decode.field "scrap" Scrap.decoder)
        (Decode.field "archive" Archive.decoder)
        (Decode.field "arrangements" (Decode.list Arrangement.decoder))
        (Decode.succeed False)


default timestamp =
    { desk = Desk.default
    , slipbox = Slipbox.default
    , scrap = Scrap.default timestamp
    , archive = Archive.default
    , arrangements = []
    , modified = False
    }


modifyDesk f workspace =
    { workspace | desk = f workspace.desk, modified = True }


modifySlipbox f workspace =
    { workspace | slipbox = f workspace.slipbox, modified = True }


modifyScrap f workspace =
    { workspace | scrap = f workspace.scrap, modified = True }


modifyArchive f workspace =
    { workspace | archive = f workspace.archive, modified = True }

setModifiedFalse workspace =
    { workspace | modified = False }

addArrangement arrangement workspace =
    { workspace | arrangements = arrangement :: workspace.arrangements}

removeArrangement arrangement workspace =
    { workspace | arrangements = List.filter (\x -> x /= arrangement) workspace.arrangements }

toEncodedString workspace =
    encode workspace
    |> Encode.encode 4

findFirstItem p xs =
    let
        loop remaining =
            case remaining of
                h :: tail ->
                    if p h then
                        Just h
                    else
                        loop tail
                [] ->
                    Nothing
    in
    loop xs

unwrapMaybes xs =
    let
        loop acc remaining =
            case remaining of
                h :: tail ->
                    case h of
                        Just v ->
                            loop (v :: acc) tail
                        Nothing ->
                            loop acc tail
                [] ->
                    List.reverse acc
    in
    loop [] xs


applyArrangement : Time.Posix -> Arrangement -> Workspace -> Workspace
applyArrangement now arrangement ({desk, slipbox} as workspace) =
    let
        arrSet =
            Set.fromList (List.map (.created >> posixToMillis) arrangement.cards)
        findCardAndConvert meta =
            case findFirstItem (\c -> posixToMillis c.created == posixToMillis meta.created) workspace.desk.cards of
                Nothing ->
                    case findFirstItem (\c -> posixToMillis c.created == posixToMillis meta.created) workspace.slipbox.cards of
                        Just c ->
                            Just <| Card.toDesk c meta.position meta.zIndex
                        Nothing ->
                            Nothing
                Just c ->
                    Just { c | position = meta.position, zIndex = meta.zIndex }
        deskToSlipbox =
            workspace.desk.cards
            |> List.filter (\c -> not <| Set.member (posixToMillis c.created) arrSet)
        newSlipboxCards =
            workspace.slipbox.cards
            |> List.filter (\c -> not <| Set.member (posixToMillis c.created) arrSet)
            |> List.append (List.map (\c -> Card.toSlipbox c now) deskToSlipbox)
        newSlipbox =
            { slipbox | cards = newSlipboxCards }
        newDesk =
            { desk | cards = List.map findCardAndConvert arrangement.cards |> unwrapMaybes }
    in
        { workspace | desk = newDesk, slipbox = newSlipbox }
