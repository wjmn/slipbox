module Main exposing (..)

import Archive
import Archived
import Arrangement exposing (Arrangement)
import Browser
import Browser.Dom as Dom
import Card
import Colour exposing (Colour)
import Desk
import File exposing (File)
import File.Download
import File.Select
import Fuzzy
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Position exposing (Position)
import Random
import Scrap exposing (Scrap)
import Set
import Slipbox
import Task
import Time exposing (Posix, posixToMillis)
import Workspace exposing (..)
import Browser.Events exposing (onResize)



---- MODEL ----


cardHeight =
    240


cardWidth =
    320


headerHeight =
    10


editingCardId =
    "current-editing-card-textarea"


clipPosition bounds { x, y } =
    { x = Basics.min (Basics.max x (cardWidth / 2)) (bounds.width - cardWidth / 2)
    , y = bounds.y + Basics.min (Basics.max y (cardHeight / 2)) (bounds.height - cardHeight / 2)
    }


type Model
    = StateStartupNoTimestampYet
    | StateMain DataMain
    | StateArchive DataMain DataArchive


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


decoderScrapClick msg =
    Decode.at [ "button" ] Decode.int
        |> Decode.map
            (\x ->
                if x == 1 then
                    ( msg, True )

                else
                    ( NoOp, False )
            )


decoderClickedArchived msg =
    Decode.at [ "button" ] Decode.int
        |> Decode.map
            (\x ->
                if x == 1 then
                    ( msg, True )

                else
                    ( NoOp, False )
            )


decoderArrangementButton arrangement =
    Decode.at [ "button" ] Decode.int
        |> Decode.map
            (\x ->
                if x == 0 then
                    ( ClickedArrangement arrangement, True )

                else if x == 1 then
                    ( ClickedRemoveArrangement arrangement, True )

                else
                    ( NoOp, True )
            )


generatorCentrePoint bounds =
    let
        midCardWidth =
            cardWidth / 2

        midCardHeight =
            cardHeight / 2
    in
    Random.map2 Position
        (Random.float midCardWidth (bounds.width - midCardWidth))
        (Random.float midCardHeight (bounds.height - midCardHeight) |> Random.map ((+) bounds.y))


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
    , slipboxSearch : String
    , colourFilter : Maybe Colour
    , arrangementName : String
    }


type alias DataArchive =
    { search : String }


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
        , slipboxSearch = ""
        , colourFilter = Nothing
        , arrangementName = ""
        }


defaultDataArchive =
    { search = "" }


init : ( Model, Cmd Msg )
init =
    ( StateStartupNoTimestampYet, Task.perform GotStartupTimestamp Time.now )



---- UPDATE ----


type Msg
    = GotStartupTimestamp Posix
    | OnWindowResize Int Int
    | GotDeskBoundingRect (Result Dom.Error Dom.Element)
    | ChangedFilename String
    | ChangedScrapContent String
    | ClickedSaveWorkspace
    | ClickedOpenWorkspace
    | FileSelected File
    | FileRead String String
    | OnDeskMouseMove { desk : Position, card : Position }
    | OnDeskMouseLeave
    | ClickedDesk Position
    | CreateCardWithTimestamp Position Posix
    | ChangedEditingCardContent String
    | ClickedOutsideEditingCard Bool
    | MoveCardToSlipbox Card.OnDesk Posix
    | MoveCardToArchive Card.OnDesk Posix
    | MouseDownCard Position Card.OnDesk MouseInfo
    | MouseUpCard Position Card.OnDesk
    | ClickedSlipboxCard Card.InSlipbox
    | GotRandomPosition Card.InSlipbox Float Position
    | ClickedViewArchive
    | ClickedViewMain
    | ClickedArchiveScrap Scrap
    | MoveScrapToArchive Scrap Posix
    | ClickedCardInArchive Card.InSlipbox
    | ClickedScrapInArchive Scrap
    | ClickedScrapInArchiveWithTime Scrap Posix
    | ChangedArchiveSearch String
    | ClickedCycleCardColour Posix
    | ChangedSlipboxSearch String
    | ClickedCycleSortOrder
    | ClickedCycleColourFilter
    | ChangedArrangementTitle String
    | ClickedSaveArrangement String Desk.Desk
    | ClickedArrangement Arrangement
    | GotArrangementTime Arrangement Posix
    | ClickedRemoveArrangement Arrangement
    | OnBeforeUnload
    | NoOp


withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd msg model =
    ( model, msg )


inMain : Model -> (DataMain -> DataMain) -> Model
inMain model f =
    case model of
        StateMain data ->
            StateMain (f data)

        StateArchive data a ->
            StateArchive (f data) a

        _ ->
            model



inArchive : Model -> (DataArchive -> DataArchive) -> Model
inArchive model f =
    case model of
        StateArchive data a ->
            StateArchive data (f a)
        _ ->
            model

fromMain : Model -> (DataMain -> a) -> Maybe a
fromMain model f =
    case model of
        StateMain data ->
            Just (f data)

        StateArchive data _ ->
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


withSlipboxSearch : String -> Model -> Model
withSlipboxSearch search model =
    (\d -> { d | slipboxSearch = search })
        |> inMain model


withArrangementName : String -> Model -> Model
withArrangementName string model =
    (\d -> { d | arrangementName = string })
        |> inMain model


withColourFilter : Maybe Colour -> Model -> Model
withColourFilter colour model =
    (\d -> { d | colourFilter = colour })
        |> inMain model

withArchiveSearch : String -> Model -> Model
withArchiveSearch string model =
    (\d -> { d | search = string })
        |> inArchive model

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
        OnWindowResize _ _ ->
            model
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
                positionAdjusted bounds centreOffset =
                    Position.minus position.desk centreOffset
                        |> (\p ->
                                Position.minus p { x = 0, y = headerHeight }
                                    |> clipPosition bounds
                           )

                modelDragged =
                    case ( .currentDraggingCard |> fromMain model, .deskBoundingRect |> fromMain model ) of
                        ( Just (Just { card, centreOffset }), Just bounds ) ->
                            model
                                |> modifyDraggingCard (\c -> { c | card = Card.withPosition (positionAdjusted bounds centreOffset) c.card })

                        _ ->
                            model
            in
            modelDragged
                |> withDeskPosition position.desk
                |> withCardPosition position.card
                |> withCmd Cmd.none

        OnDeskMouseLeave ->
            case .currentDraggingCard |> fromMain model of
                Just (Just { card }) ->
                    Workspace.modifyDesk (Desk.withNewCard card)
                        |> inWorkspaceOf model
                        |> withDraggingCard Nothing
                        |> withCmd Cmd.none

                _ ->
                    model
                        |> withCmd Cmd.none

        ClickedDesk position ->
            model
                |> withCmd (Task.perform (CreateCardWithTimestamp position) Time.now)

        CreateCardWithTimestamp position now ->
            let
                adjustedPosition =
                    case .deskBoundingRect |> fromMain model of
                        Just bounds ->
                            clipPosition bounds position

                        _ ->
                            position
            in
            case .currentEditingCard |> fromMain model of
                Just (Just card) ->
                    Workspace.modifyDesk (Desk.withNewCard card)
                        |> inWorkspaceOf model
                        |> withEditingCard (Just <| Card.newOnDesk now adjustedPosition)
                        |> withCmd (Task.attempt (\_ -> NoOp) (Dom.focus editingCardId))

                _ ->
                    model
                        |> withEditingCard (Just <| Card.newOnDesk now adjustedPosition)
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
            Workspace.modifySlipbox (Slipbox.withCard card now)
                |> inWorkspaceOf model
                |> withDraggingCard Nothing
                |> withCmd Cmd.none

        MoveCardToArchive card now ->
            Workspace.modifyArchive (Archive.withCard card now)
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

                        else if mouseDownInfo.button == 1 then
                            model
                                |> withCmd (Task.perform (MoveCardToArchive card) Time.now)

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
            case ( .workspace >> .desk |> fromMain model, .deskBoundingRect |> fromMain model ) of
                ( Just desk, Just bounds ) ->
                    let
                        maxZ =
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

        ClickedViewArchive ->
            case model of
                StateMain data ->
                    StateArchive data defaultDataArchive
                        |> withCmd Cmd.none

                _ ->
                    model
                        |> withCmd Cmd.none

        ClickedViewMain ->
            case model of
                StateArchive data _ ->
                    StateMain data
                        |> withCmd Cmd.none

                _ ->
                    model
                        |> withCmd Cmd.none

        ClickedArchiveScrap scrap ->
            model
                |> withCmd (Task.perform (MoveScrapToArchive scrap) Time.now)

        MoveScrapToArchive scrap now ->
            (Workspace.modifyArchive (Archive.withScrap scrap now)
                >> Workspace.modifyScrap (\_ -> Scrap.default now)
            )
                |> inWorkspaceOf model
                |> withDraggingCard Nothing
                |> withCmd Cmd.none

        ClickedCardInArchive card ->
            Workspace.modifySlipbox (Slipbox.withCard card card.modified)
                >> Workspace.modifyArchive (Archive.withoutCard card)
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        ClickedScrapInArchive scrap ->
            model
                |> withCmd (Task.perform (ClickedScrapInArchiveWithTime scrap) Time.now)

        ClickedScrapInArchiveWithTime scrap now ->
            Workspace.modifyScrap (\_ -> scrap)
                >> Workspace.modifyArchive (Archive.withoutScrap scrap)
                >> Workspace.modifyArchive (Archive.withScrap (Maybe.withDefault (Scrap.default now) <| fromMain model (\x -> x.workspace.scrap)) now)
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        ChangedArchiveSearch string ->
            model
            |> withArchiveSearch string
            |> withCmd Cmd.none

        ClickedCycleCardColour posix ->
            case .currentEditingCard |> fromMain model of
                Just (Just c) ->
                    if c.created == posix then
                        model
                            |> withEditingCard (Just (Card.cycleColour c))
                            |> withCmd Cmd.none

                    else
                        Workspace.modifyDesk (Desk.modifyCard posix Card.cycleColour)
                            |> inWorkspaceOf model
                            |> withCmd Cmd.none

                _ ->
                    Workspace.modifyDesk (Desk.modifyCard posix Card.cycleColour)
                        |> inWorkspaceOf model
                        |> withCmd Cmd.none

        ChangedSlipboxSearch string ->
            model
                |> withSlipboxSearch string
                |> withCmd Cmd.none

        ClickedCycleSortOrder ->
            Workspace.modifySlipbox Slipbox.cycleSortOrder
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        ClickedCycleColourFilter ->
            case .colourFilter |> fromMain model of
                Just colour ->
                    model
                        |> withColourFilter (Colour.cycleMaybe colour)
                        |> withCmd Cmd.none

                _ ->
                    model |> withCmd Cmd.none

        ChangedArrangementTitle string ->
            model
                |> withArrangementName string
                |> withCmd Cmd.none

        ClickedSaveArrangement name desk ->
            if String.trim name /= "" then
                Workspace.addArrangement (Arrangement.fromDesk name desk)
                    |> inWorkspaceOf model
                    |> withArrangementName ""
                    |> withCmd Cmd.none

            else
                model |> withCmd Cmd.none

        ClickedArrangement arrangement ->
            model
                |> withCmd (Task.perform (GotArrangementTime arrangement) Time.now)

        GotArrangementTime arrangement posix ->
            Workspace.applyArrangement posix arrangement
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        ClickedRemoveArrangement arrangement ->
            Workspace.removeArrangement arrangement
                |> inWorkspaceOf model
                |> withCmd Cmd.none

        OnBeforeUnload ->
            Workspace.setModifiedFalse
                |> inWorkspaceOf model
                |> withCmd (downloadData |> onDataMainOf model)

        NoOp ->
            model
                |> withCmd Cmd.none



---- VIEW ----


viewColourPin card =
    let
        character =
            case card.colour of
                Colour.White ->
                    "◇"

                _ ->
                    "◆"

        colorHex =
            case card.colour of
                Colour.White ->
                    "inherit"

                _ ->
                    Colour.toHex card.colour
    in
    div
        [ class "colour-pin"
        , style "color" colorHex
        , preventDefaultOn "mousedown" (Decode.succeed ( ClickedCycleCardColour card.created, True ))
        ]
        [ text character ]


cardHeader card =
    div [ class "card-header", preventDefaultOn "mousedown" (Decode.succeed (NoOp, True)) ]
        [ viewColourPin card ]


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
                ]
                [ cardHeader card
                , div
                    [ class "card-content"
                    , preventDefaultOn "mousedown" (decoderMouseDown (MouseDownCard data.deskPosition card) |> Decode.map (\x -> ( x, True )))
                    ]
                    [ text card.content ]
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
                        [ cardHeader card
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
                        ]
                        [ cardHeader card
                        , div
                            [ class "card-content"
                            , preventDefaultOn "contextmenu" (Decode.succeed ( NoOp, True ))
                            ]
                            [ text card.content ]
                        ]
    in
    div
        [ class "desk-cards" ]
        (viewDragging :: viewEditing :: List.map viewCard data.workspace.desk.cards)

matchingChars matches =
    matches
        |> List.concatMap (\m -> List.map (\x -> x + m.offset) m.keys)
        |> Set.fromList

segment matchSet chars =
    let
        loop acc groups remaining =
            case ( acc, remaining ) of
                ( ( cum, bool ), ( i, c ) :: rest ) ->
                    if bool == Set.member i matchSet then
                        loop ( c :: cum, bool ) groups rest

                    else
                        loop ( [ c ], not bool ) (( String.fromList <| List.reverse cum, bool ) :: groups) rest

                ( ( cum, bool ), [] ) ->
                    ( String.fromList <| List.reverse cum, bool )
                        :: groups
                        |> List.reverse

        enumerated =
            List.indexedMap (\i x -> ( i, x )) chars
    in
    loop ( [], True ) [] enumerated


matchToMarkedHtml card matches =
    let
        matchSet = matchingChars matches
    in
    String.toList card.content
        |> segment matchSet
        |> List.map
            (\( string, mbool ) ->
                if mbool then
                    span [ class "search-match" ] [ text string ]

                else
                    text string
            )
        |> (\x -> ( card, x ))


viewSlipboxCards : DataMain -> Html Msg
viewSlipboxCards data =
    let
        styleCardWidth =
            style "width" <| String.fromFloat cardWidth ++ "px"

        styleCardHeight =
            style "height" <| String.fromFloat cardHeight ++ "px"

        styleCardTop index =
            style "top" <| String.fromInt (40 * index) ++ "px"

        viewCard index ( card, cardContentHtml ) =
            div
                [ class "card in-slipbox"
                , styleCardWidth
                , styleCardHeight
                , styleCardTop index
                , onClick (ClickedSlipboxCard card)
                ]
                [ cardHeader card
                , div [ class "card-content" ] cardContentHtml
                ]

        filtered =
            case data.colourFilter of
                Nothing ->
                    data.workspace.slipbox

                Just c ->
                    Slipbox.filterByColour c data.workspace.slipbox

        orderedCards =
            case data.slipboxSearch of
                "" ->
                    Slipbox.viewBySortOrder filtered
                        |> List.map (\c -> ( c, [ text c.content ] ))

                search ->
                    List.map (\c -> ( c, Fuzzy.match [] [ " ", "\n", "." ] (String.toLower search) (String.toLower c.content) )) filtered.cards
                        |> List.sortBy (\( c, m ) -> m.score)
                        |> List.map (\( c, m ) -> matchToMarkedHtml c m.matches)
    in
    div [ class "slipbox-cards" ]
        (List.indexedMap viewCard orderedCards)


viewMainLeft data =
    let
        colourFilterText =
            case data.colourFilter of
                Nothing ->
                    div [ style "color" "transparent" ] [ text "◆" ]

                Just c ->
                    case c of
                        Colour.White ->
                            div [] [ text "◇" ]

                        _ ->
                            div [ style "color" (Colour.toHex c) ] [ text "◆" ]

        colourModified =
            if data.workspace.modified then
                style "color" "red"
            else
                style "color" "inherit"
    in
    div
        [ class "panel-container"
        ]
        [ div [ class "top-bar workspace-details" ]
            [ input [ class "filename-input", type_ "text", onInput ChangedFilename, value data.filename, colourModified ] []
            , button [ class "open-workspace-button", onClick ClickedOpenWorkspace ] [ text "Open" ]
            , button [ class "save-workspace-button", onClick ClickedSaveWorkspace ] [ text "Save as" ]
            ]
        , div [ class "middle-bar" ]
            [ button [ class "colour-filter-button", onClick ClickedCycleColourFilter ] [ colourFilterText ]
            , button [ class "search-order-button", onClick ClickedCycleSortOrder ] [ text <| Slipbox.sortOrderToString data.workspace.slipbox.sortOrder ]
            , input [ class "search-string-input", type_ "text", onInput ChangedSlipboxSearch, value data.slipboxSearch ] []
            ]
        , div [ class "slipbox-container" ]
            [ viewSlipboxCards data ]
        ]


viewMainMiddle data =
    div
        [ class "panel-container"
        ]
        [ div [ class "top-bar" ]
            [ button [ onClick ClickedViewArchive ] [ text "View Archive" ] ]
        , div
            [ class "scrap-container"
            ]
            [ textarea
                [ class "scrap"
                , onInput ChangedScrapContent
                , preventDefaultOn "mouseup" (decoderScrapClick (ClickedArchiveScrap data.workspace.scrap))
                , value data.workspace.scrap.content
                ]
                []
            ]
        ]


viewMainRight : DataMain -> Html Msg
viewMainRight data =
    let
        arrangementToHtml arrangement =
            button
                [ class "arrangement-button"
                , preventDefaultOn "mouseup" (decoderArrangementButton arrangement)
                ]
                [ text arrangement.name ]

        arrangementHtmlList =
            List.map arrangementToHtml <| List.sortBy .name data.workspace.arrangements
    in
    div
        [ class "panel-container"
        ]
        [ div [ class "top-bar" ]
            [ button [ class "arrangement-pin-button", onClick (ClickedSaveArrangement data.arrangementName data.workspace.desk) ] [ text "Pin" ]
            , input [ class "arrangement-name-input", onInput ChangedArrangementTitle, type_ "text" ] []
            , div [ class "arrangement-buttons" ] arrangementHtmlList
            ]
        , div
            [ id "desk"
            , preventDefaultOn "contextmenu" (Decode.succeed ( NoOp, True ))
            , preventDefaultOn "mousemove" (Decode.map OnDeskMouseMove (decoderMouseMove data.deskBoundingRect) |> Decode.map (\x -> ( x, True )))
            , onMouseLeave OnDeskMouseLeave
            ]
            [ div
                [ class "desk"
                , onClick (ClickedDesk data.deskPosition)
                , preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))
                ]
                []
            , viewCardsOnDesk data
            ]
        ]


viewArchive : DataMain -> DataArchive -> Html Msg
viewArchive data a =
    let
        styleCardWidth =
            style "width" <| String.fromFloat cardWidth ++ "px"

        styleCardHeight =
            style "height" <| String.fromFloat cardHeight ++ "px"

        viewArchiveItem (item, contentHtml) =
            case item.item of
                Archived.Card card ->
                    div
                        [ class "archived card"
                        , styleCardWidth
                        , styleCardHeight
                        ]
                        [ cardHeader card
                        , div
                            [ class "card-content"
                            , preventDefaultOn "mouseup" (decoderClickedArchived (ClickedCardInArchive card))
                            ]
                            contentHtml
                        ]

                Archived.Scrap scrap ->
                    div
                        [ class "archived scrap"
                        , preventDefaultOn "mouseup" (decoderClickedArchived (ClickedScrapInArchive scrap))
                        ]
                        [ div [] contentHtml ]

        defaultContent item =
            case item.item of
                Archived.Card card ->
                    card.content
                Archived.Scrap scrap ->
                    scrap.content

        toMarkedHtml item matches =
            case item.item of
                Archived.Card card ->
                    matchToMarkedHtml card matches
                    |> (\ (c, h) -> ({ item = Archived.Card c}, h))
                Archived.Scrap scrap ->
                    matchToMarkedHtml scrap matches
                    |> (\ (s, h) -> ({ item = Archived.Scrap s}, h))

        ordered =
            case a.search of
                "" ->
                    List.map (\x -> viewArchiveItem (x, [text <| defaultContent x])) data.workspace.archive.items

                search ->
                    List.map (\c -> ( c, Fuzzy.match [] [ " ", ".", "\n" ] (String.toLower search) (String.toLower <| defaultContent c) )) data.workspace.archive.items
                        |> List.sortBy (\( c, m ) -> m.score)
                        |> List.map (\( c, m ) -> toMarkedHtml c m.matches)
                        |> List.map viewArchiveItem

    in
    div
        [ class "archive-grid" ]
        ordered


viewCase : Model -> Html Msg
viewCase model =
    case model of
        StateStartupNoTimestampYet ->
            div [ class "loading" ] [ text "Loading..." ]

        StateMain data ->
            div [ class "main-workspace" ]
                [ div
                    [ class "left main-panel"
                    , on "mousedown" (Decode.map ClickedOutsideEditingCard decoderClickedContainer)
                    ]
                    [ viewMainLeft data ]
                , div
                    [ class "middle main-panel"
                    , on "click" (Decode.map ClickedOutsideEditingCard decoderClickedContainer)
                    ]
                    [ viewMainMiddle data ]
                , div [ class "right main-panel" ] [ viewMainRight data ]
                ]

        StateArchive data a ->
            div [ class "archive-view" ]
                [ div [ class "top-bar" ]
                    [ button [ onClick ClickedViewMain ] [ text "View Main" ]
                    , input [onInput ChangedArchiveSearch, class "archive-search", type_ "text", value a.search] []]
                , div [ class "archive-grid-container" ] [ viewArchive data a ]
                ]


view : Model -> Html Msg
view model =
    let
        isModified =
            (.workspace >> .modified) |> fromMain model

        wrapUnload attrs =
            case isModified of
                Just True ->
                    on "beforeunload" (Decode.succeed OnBeforeUnload) :: attrs

                _ ->
                    attrs
    in
    div
        (wrapUnload [ class "container" ])
        [ viewCase model ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions =  always (onResize OnWindowResize)
        }
