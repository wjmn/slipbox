module Workspace exposing (..)

import Archive exposing (Archive)
import Desk exposing (Desk)
import Json.Decode as Decode
import Json.Encode as Encode
import Scrap exposing (Scrap)
import Slipbox exposing (Slipbox)


type alias Workspace =
    { desk : Desk
    , slipbox : Slipbox
    , scrap : Scrap
    , archive : Archive
    , modified : Bool
    }


encode workspace =
    Encode.object
        [ ( "desk", Desk.encode workspace.desk )
        , ( "slipbox", Slipbox.encode workspace.slipbox )
        , ( "scrap", Scrap.encode workspace.scrap )
        , ( "archive", Archive.encode workspace.archive)
        ]


decoder =
    Decode.map5 Workspace
        (Decode.field "desk" Desk.decoder)
        (Decode.field "slipbox" Slipbox.decoder)
        (Decode.field "scrap" Scrap.decoder)
        (Decode.field "archive" Archive.decoder)
        (Decode.succeed False)


default timestamp =
    { desk = Desk.default
    , slipbox = Slipbox.default
    , scrap = Scrap.default timestamp
    , archive = Archive.default
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

toEncodedString workspace =
    encode workspace
    |> Encode.encode 4
