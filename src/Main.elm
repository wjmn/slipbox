module Main exposing (..)

import Browser
import File exposing (File)
import File.Download
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Scrap
import Task
import Time exposing (Posix)
import Workspace exposing (..)



---- MODEL ----


type Model
    = StateStartupNoTimestampYet
    | StateMain DataMain


type Error
    = ErrorDecodingJson Decode.Error


type alias DataMain =
    { workspace : Workspace
    , filename : String
    , error : Maybe Error
    }


init : ( Model, Cmd Msg )
init =
    ( StateStartupNoTimestampYet, Task.perform GotStartupTimestamp Time.now )



---- UPDATE ----


type Msg
    = GotStartupTimestamp Posix
    | ChangedFilename String
    | ChangedScrapContent String
    | ClickedSaveWorkspace
    | ClickedOpenWorkspace
    | FileSelected File
    | FileRead String String
    | NoOp


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd msg model =
    ( model, msg )


inWorkspaceOf : Model -> (Workspace -> Workspace) -> Model
inWorkspaceOf model f =
    case model of
        StateMain data ->
            StateMain { data | workspace = f data.workspace }

        _ ->
            model


injectWorkspace : String -> Workspace -> Model -> Model
injectWorkspace filename workspace model =
    case model of
        StateMain data ->
            StateMain { data | workspace = workspace, filename = filename }

        _ ->
            model


withFilename : String -> Model -> Model
withFilename filename model =
    case model of
        StateMain data ->
            StateMain { data | filename = filename }

        _ ->
            model


withError : Error -> Model -> Model
withError error model =
    case model of
        StateMain data ->
            StateMain { data | error = Just error }

        _ ->
            model


onDataMainOf : Model -> (DataMain -> Cmd Msg) -> Cmd Msg
onDataMainOf model f =
    case model of
        StateMain dataMain ->
            f dataMain

        _ ->
            Cmd.none


downloadData : DataMain -> Cmd Msg
downloadData data =
    File.Download.string
        data.filename
        "application/json"
        (Workspace.toEncodedString data.workspace)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStartupTimestamp posix ->
            StateMain
                { workspace = Workspace.default posix
                , filename = "workspace_default.json"
                , error = Nothing
                }
                |> withCmd Cmd.none

        ChangedFilename string ->
            model
                |> withFilename string
                |> withCmd Cmd.none

        ChangedScrapContent string ->
            Workspace.modifyScrap (Scrap.withContent string)
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        ClickedSaveWorkspace ->
            Workspace.setModifiedFalse
                |> inWorkspaceOf model
                |> withCmd (downloadData |> onDataMainOf model)

        ClickedOpenWorkspace ->
            model
                |> withCmd (File.Select.file [ "application/json" ] FileSelected)

        FileSelected file ->
            model
                |> withCmd (Task.perform (FileRead (File.name file)) (File.toString file))

        FileRead filename contents ->
            case Decode.decodeString Workspace.decoder contents of
                Ok workspace ->
                    model
                        |> injectWorkspace filename workspace
                        |> withCmd Cmd.none

                Err e ->
                    model
                        |> withError (ErrorDecodingJson e)
                        |> withCmd Cmd.none

        NoOp ->
            model
                |> withCmd Cmd.none



---- VIEW ----


viewMainLeft data =
    div [ class "panel-container" ]
        [ div [ class "top-bar" ]
            [ input [ type_ "text", onInput ChangedFilename, value data.filename ] []
            , button [ class "open-workspace-button", onClick ClickedOpenWorkspace ] [ text "Open Workspace" ]
            , button [ class "save-workspace-button", onClick ClickedSaveWorkspace ] [ text "Save Workspace" ]
            ]
        ]


viewMainMiddle data =
    div [ class "panel-container" ]
        [ div [ class "top-bar" ] []
        , div [ class "scrap-container" ]
            [ textarea
                [ class "scrap"
                , onInput ChangedScrapContent
                , value data.workspace.scrap.content
                ]
                []
            ]
        ]


viewMainRight data =
    div [ class "panel-container" ] []


viewCase model =
    case model of
        StateStartupNoTimestampYet ->
            div [ class "loading" ] [ text "Loading..." ]

        StateMain data ->
            div [ class "main-workspace" ]
                [ div [ class "left main-panel" ] [ viewMainLeft data ]
                , div [ class "middle main-panel" ] [ viewMainMiddle data ]
                , div [ class "right main-panel" ] [ viewMainRight data ]
                ]


view : Model -> Html Msg
view model =
    div [ class "container" ] [ viewCase model ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
