module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Card
import Desk
import File exposing (File)
import File.Download
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Position exposing (Position)
import Scrap
import Slipbox
import Task
import Time exposing (Posix)
import Workspace exposing (..)
import Random
import Time exposing (posixToMillis)



---- MODEL ----


cardHeight =
    240


cardWidth =
    320


editingCardId =
    "current-editing-card-textarea"


type Model
    = StateStartupNoTimestampYet
    | StateMain DataMain


type Error
    = ErrorDecodingJson Decode.Error


decoderMouseMove bounds =
    Decode.map4 (mouseMoveToPosition bounds)
        (Decode.at [ "clientX" ] Decode.int)
        (Decode.at [ "clientY" ] Decode.int)
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)


mouseMoveToPosition bounds cX cY oX oY =
    { desk = { x = toFloat cX - bounds.x, y = toFloat cY - bounds.y }
    , card = { x = toFloat oX - cardWidth / 2, y = toFloat oY - cardHeight / 2 }
    }


decoderClickedContainer =
    Decode.at [ "target", "id" ] Decode.string
        |> Decode.map ((/=) editingCardId)


rightClickDecoder msg =
    Decode.succeed msg
        |> Decode.map (\m -> ( m, True ))


decoderMouseDown msg =
    Decode.map msg
        (Decode.map MouseInfo
            (Decode.at [ "button" ] Decode.int)
        )

generatorCentrePoint bounds =
    let
        midCardWidth = cardWidth / 2
        midCardHeight = cardHeight / 2
    in
    Random.map2 Position
        (Random.float (midCardWidth) (bounds.width - midCardWidth))
        (Random.float (midCardHeight) (bounds.height - midCardHeight))

type alias MouseInfo =
    { button : Int }


type alias DataMain =
    { workspace : Workspace
    , filename : String
    , error : Maybe Error
    , deskBoundingRect : { x : Float, y : Float, width : Float, height : Float }
    , deskPosition : Position
    , cardPosition : Position
    , mouseDownInfo : Maybe { position : Position, button : Int }
    , currentEditingCard : Maybe Card.OnDesk
    , currentDraggingCard : Maybe { card : Card.OnDesk, centreOffset : Position }
    }


makeInitialState posix =
    StateMain
        { workspace = Workspace.default posix
        , filename = "workspace_default.json"
        , error = Nothing
        , deskPosition = { x = 0, y = 0 }
        , cardPosition = { x = 0, y = 0 }
        , mouseDownInfo = Nothing
        , currentEditingCard = Nothing
        , currentDraggingCard = Nothing
        , deskBoundingRect = { x = 0, y = 0, width = 0, height = 0 }
        }


init : ( Model, Cmd Msg )
init =
    ( StateStartupNoTimestampYet, Task.perform GotStartupTimestamp Time.now )



---- UPDATE ----


type Msg
    = GotStartupTimestamp Posix
    | GotDeskBoundingRect (Result Dom.Error Dom.Element)
    | ChangedFilename String
    | ChangedScrapContent String
    | ClickedSaveWorkspace
    | ClickedOpenWorkspace
    | FileSelected File
    | FileRead String String
    | OnDeskMouseMove { desk : Position, card : Position }
    | ClickedDesk Position
    | CreateCardWithTimestamp Position Posix
    | ChangedEditingCardContent String
    | ClickedOutsideEditingCard Bool
    | MoveCardToSlipbox Card.OnDesk Posix
    | MouseDownCard Position Card.OnDesk MouseInfo
    | MouseUpCard Position Card.OnDesk
    | ClickedSlipboxCard Card.InSlipbox
    | GotRandomPosition Card.InSlipbox Float Position
    | NoOp


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd msg model =
    ( model, msg )


inMain : Model -> (DataMain -> DataMain) -> Model
inMain model f =
    case model of
        StateMain data ->
            StateMain (f data)

        _ ->
            model


fromMain : Model -> (DataMain -> a) -> Maybe a
fromMain model f =
    case model of
        StateMain data ->
            Just (f data)

        _ ->
            Nothing


inWorkspaceOf : Model -> (Workspace -> Workspace) -> Model
inWorkspaceOf model f =
    (\d -> { d | workspace = f d.workspace })
        |> inMain model


injectWorkspace : String -> Workspace -> Model -> Model
injectWorkspace filename workspace model =
    (\d -> { d | workspace = workspace, filename = filename })
        |> inMain model


withFilename : String -> Model -> Model
withFilename filename model =
    (\d -> { d | filename = filename })
        |> inMain model


withDeskBoundingRect : { x : Float, y : Float, width : Float, height : Float } -> Model -> Model
withDeskBoundingRect deskBoundingRect model =
    (\d -> { d | deskBoundingRect = deskBoundingRect })
        |> inMain model


withError : Error -> Model -> Model
withError error model =
    (\d -> { d | error = Just error })
        |> inMain model


withEditingCard : Maybe Card.OnDesk -> Model -> Model
withEditingCard card model =
    (\d -> { d | currentEditingCard = card })
        |> inMain model


modifyEditingCard : (Card.OnDesk -> Card.OnDesk) -> Model -> Model
modifyEditingCard f model =
    (\d -> { d | currentEditingCard = Maybe.map f d.currentEditingCard })
        |> inMain model


withDraggingCard : Maybe { card : Card.OnDesk, centreOffset : Position } -> Model -> Model
withDraggingCard card model =
    (\d -> { d | currentDraggingCard = card })
        |> inMain model


modifyDraggingCard : ({ card : Card.OnDesk, centreOffset : Position } -> { card : Card.OnDesk, centreOffset : Position }) -> Model -> Model
modifyDraggingCard f model =
    (\d -> { d | currentDraggingCard = Maybe.map f d.currentDraggingCard })
        |> inMain model


withDeskPosition : Position -> Model -> Model
withDeskPosition deskPosition model =
    (\d -> { d | deskPosition = deskPosition })
        |> inMain model


withCardPosition : Position -> Model -> Model
withCardPosition cardPosition model =
    (\d -> { d | cardPosition = cardPosition })
        |> inMain model


withMouseDownInfo : Maybe { position : Position, button : Int } -> Model -> Model
withMouseDownInfo mouseDownInfo model =
    (\d -> { d | mouseDownInfo = mouseDownInfo })
        |> inMain model


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
            makeInitialState posix
                |> withCmd (Task.attempt GotDeskBoundingRect (Dom.getElement "desk"))

        GotDeskBoundingRect element ->
            case element of
                Ok el ->
                    model
                        |> withDeskBoundingRect el.element
                        |> withCmd Cmd.none

                Err _ ->
                    model
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

        OnDeskMouseMove position ->
            let
                modelDragged =
                    case .currentDraggingCard |> fromMain model of
                        Just (Just { card, centreOffset }) ->
                            model
                                |> modifyDraggingCard (\c -> { c | card = Card.withPosition (Position.minus position.desk centreOffset) c.card })

                        _ ->
                            model
            in
            modelDragged
                |> withDeskPosition position.desk
                |> withCardPosition position.card
                |> withCmd Cmd.none

        ClickedDesk position ->
            model
                |> withCmd (Task.perform (CreateCardWithTimestamp position) Time.now)

        CreateCardWithTimestamp position now ->
            case .currentEditingCard |> fromMain model of
                Just (Just card) ->
                    Workspace.modifyDesk (Desk.withNewCard card)
                        |> inWorkspaceOf model
                        |> withEditingCard (Just <| Card.newOnDesk now position)
                        |> withCmd (Task.attempt (\_ -> NoOp) (Dom.focus editingCardId))

                _ ->
                    model
                        |> withEditingCard (Just <| Card.newOnDesk now position)
                        |> withCmd (Task.attempt (\_ -> NoOp) (Dom.focus editingCardId))

        ChangedEditingCardContent string ->
            model
                |> modifyEditingCard (Card.withContent string)
                |> withCmd Cmd.none

        ClickedOutsideEditingCard bool ->
            case ( bool, .currentEditingCard |> fromMain model ) of
                ( True, Just (Just card) ) ->
                    Workspace.modifyDesk (Desk.withNewCard card)
                        |> inWorkspaceOf model
                        |> withEditingCard Nothing
                        |> withCmd Cmd.none

                _ ->
                    model
                        |> withCmd Cmd.none

        MoveCardToSlipbox card now ->
            Workspace.modifyDesk (Desk.withoutCard card)
                >> Workspace.modifySlipbox (Slipbox.withCard card now)
                |> inWorkspaceOf model
                |> withDraggingCard Nothing
                |> withCmd Cmd.none

        MouseDownCard position card info ->
            let
                modelUnfocused =
                    case .currentEditingCard |> fromMain model of
                        Just (Just editCard) ->
                            Workspace.modifyDesk (Desk.withNewCard editCard)
                                |> inWorkspaceOf model
                                |> withEditingCard Nothing

                        _ ->
                            model

                centreOffset =
                    Maybe.withDefault { x = 0, y = 0 } (.cardPosition |> fromMain model)
            in
            Workspace.modifyDesk (Desk.withoutCard card)
                |> inWorkspaceOf modelUnfocused
                |> withDraggingCard (Just { card = card, centreOffset = centreOffset })
                |> withMouseDownInfo (Just { position = position, button = info.button })
                |> withCmd Cmd.none

        MouseUpCard position card ->
            case ( .currentDraggingCard |> fromMain model, .mouseDownInfo |> fromMain model ) of
                ( Just (Just currentDragging), Just (Just mouseDownInfo) ) ->
                    if Card.matches currentDragging.card card then
                        if mouseDownInfo.button == 2 then
                            model
                                |> withCmd (Task.perform (MoveCardToSlipbox card) Time.now)

                        else if mouseDownInfo.button == 0 then
                            if Position.distance mouseDownInfo.position position < 4 then
                                model
                                    |> withDraggingCard Nothing
                                    |> withEditingCard (Just card)
                                    |> withCmd (Task.attempt (\_ -> NoOp) (Dom.focus editingCardId))

                            else
                                Workspace.modifyDesk (Desk.withNewCard card)
                                    |> inWorkspaceOf model
                                    |> withDraggingCard Nothing
                                    |> withCmd Cmd.none

                        else
                            model |> withCmd Cmd.none

                    else
                        model |> withCmd Cmd.none

                _ ->
                    model |> withCmd Cmd.none

        ClickedSlipboxCard card ->
            case (.workspace >> .desk |> fromMain model, .deskBoundingRect |> fromMain model) of
                (Just desk, Just bounds) ->
                    let maxZ =
                            List.map .zIndex desk.cards
                            |> List.maximum
                            |> Maybe.withDefault 0
                    in
                        model
                            |> withCmd (Random.generate (GotRandomPosition card maxZ) (generatorCentrePoint bounds))
                _ ->
                    model |> withCmd Cmd.none

        GotRandomPosition card maxZ position ->
            Workspace.modifyDesk (Desk.withNewCard <| Card.toDesk card position maxZ)
            >> Workspace.modifySlipbox (Slipbox.withoutCard card)
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        NoOp ->
            model
                |> withCmd Cmd.none



---- VIEW ----


viewCardsOnDesk : DataMain -> Html Msg
viewCardsOnDesk data =
    let
        styleCardTop card =
            style "top" <| String.fromFloat (card.position.y - cardHeight / 2) ++ "px"

        styleCardLeft card =
            style "left" <| String.fromFloat (card.position.x - cardWidth / 2) ++ "px"

        styleCardWidth =
            style "width" <| String.fromFloat cardWidth ++ "px"

        styleCardHeight =
            style "height" <| String.fromFloat cardHeight ++ "px"

        maxZ =
            List.map .zIndex data.workspace.desk.cards
                |> List.maximum
                |> Maybe.withDefault 0

        viewCard card =
            div
                [ class "card on-desk"
                , styleCardLeft card
                , styleCardTop card
                , styleCardWidth
                , styleCardHeight
                , style "z-index" (String.fromFloat card.zIndex)

                --, on "click" (Decode.map ClickedOutsideEditingCard decoderClickedContainer)
                , preventDefaultOn "mousedown" (decoderMouseDown (MouseDownCard data.deskPosition card) |> Decode.map (\x -> ( x, True )))
                ]
                [ div [ class "coloured-corner" ] []
                , div [ class "card-content" ] [ text card.content ]
                ]

        viewEditing =
            case data.currentEditingCard of
                Nothing ->
                    div [] []

                Just card ->
                    div
                        [ class "card on-desk editing"
                        , styleCardLeft card
                        , styleCardTop card
                        , styleCardWidth
                        , styleCardHeight
                        , style "z-index" (String.fromFloat (maxZ + 1))
                        ]
                        [ div [ class "coloured-corner" ] []
                        , textarea
                            [ class "card-content"
                            , id editingCardId
                            , value card.content
                            , onInput ChangedEditingCardContent
                            ]
                            []
                        ]

        viewDragging =
            case data.currentDraggingCard of
                Nothing ->
                    div [] []

                Just { card } ->
                    div
                        [ class "card on-desk dragging"
                        , styleCardLeft card
                        , styleCardTop card
                        , styleCardWidth
                        , styleCardHeight
                        , style "z-index" (String.fromFloat (maxZ + 1))
                        , onMouseUp (MouseUpCard data.deskPosition card)
                        , preventDefaultOn "contextmenu" (Decode.succeed ( NoOp, True ))
                        ]
                        [ div [ class "coloured-corner" ] []
                        , div [ class "card-content" ] [ text card.content ]
                        ]
    in
    div
        [ class "desk-cards" ]
        (viewDragging :: viewEditing :: List.map viewCard data.workspace.desk.cards)


viewSlipboxCards : DataMain -> Html Msg
viewSlipboxCards data =
    let
        styleCardWidth =
            style "width" <| String.fromFloat cardWidth ++ "px"

        styleCardHeight =
            style "height" <| String.fromFloat cardHeight ++ "px"

        viewCard card =
            div
                [ class "card in-slipbox"
                , styleCardWidth
                , styleCardHeight
                , onClick (ClickedSlipboxCard card)
                ]
                [ div [ class "coloured-corner" ] []
                , div [ class "card-content" ] [ text card.content ]
                ]

        orderedCards =
            List.sortBy (.created >> posixToMillis) data.workspace.slipbox.cards
    in
    div [ class "slipbox-cards" ]
        (List.map viewCard orderedCards)


viewMainLeft data =
    div
        [ class "panel-container"
        ]
        [ div [ class "top-bar workspace-details" ]
            [ input [ class "filename-input", type_ "text", onInput ChangedFilename, value data.filename ] []
            , button [ class "open-workspace-button", onClick ClickedOpenWorkspace ] [ text "📂" ]
            , button [ class "save-workspace-button", onClick ClickedSaveWorkspace ] [ text "💾" ]
            ]
        , div [ class "middle-bar" ] []
        , div [ class "slipbox-container" ]
            [ viewSlipboxCards data ]
        ]


viewMainMiddle data =
    div
        [ class "panel-container"
        ]
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


viewMainRight : DataMain -> Html Msg
viewMainRight data =
    div
        [ class "panel-container"
        , id "desk"
        , preventDefaultOn "mousemove" (Decode.map OnDeskMouseMove (decoderMouseMove data.deskBoundingRect) |> Decode.map (\x -> ( x, True )))
        ]
        [ div [ class "floating-bar" ] []
        , div
            [ class "desk"
            , onClick (ClickedDesk data.deskPosition)
            , preventDefaultOn "contextmenu" (Decode.succeed ( NoOp, True ))
            ]
            []
        , viewCardsOnDesk data
        ]


viewCase : Model -> Html Msg
viewCase model =
    case model of
        StateStartupNoTimestampYet ->
            div [ class "loading" ] [ text "Loading..." ]

        StateMain data ->
            div [ class "main-workspace" ]
                [ div
                    [ class "left main-panel"
                    , on "click" (Decode.map ClickedOutsideEditingCard decoderClickedContainer)
                    ]
                    [ viewMainLeft data ]
                , div
                    [ class "middle main-panel"
                    , on "click" (Decode.map ClickedOutsideEditingCard decoderClickedContainer)
                    ]
                    [ viewMainMiddle data ]
                , div [ class "right main-panel" ] [ viewMainRight data ]
                ]


view : Model -> Html Msg
view model =
    div
        [ class "container"
        ]
        [ viewCase model ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
