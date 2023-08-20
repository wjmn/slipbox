module Colour exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode

type Colour
    = White
    | Grey
    | Black
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Indigo
    | Violet

toHex : Colour -> String
toHex colour =
    case colour of
        White ->
            "#ffffff"
        Grey ->
            "#475569"
        Black ->
            "#020617"
        Red ->
            "#dc2626"
        Orange ->
            "#ea580c"
        Yellow ->
            "#eab308"
        Green ->
            "#16a34a"
        Blue ->
            "#2563eb"
        Indigo  ->
            "#4f46e5"
        Violet ->
            "#7c3aed"

toInt : Colour -> Int
toInt colour =
    case colour of
        White ->
            0
        Grey ->
            1
        Black ->
            2
        Red ->
            3
        Orange ->
            4
        Yellow ->
            5
        Green ->
            6
        Blue ->
            7
        Indigo  ->
            8
        Violet ->
            9

fromInt : Int -> Maybe Colour
fromInt int =
    case int of
        0 -> Just White
        1 -> Just Grey
        2 -> Just Black
        3 -> Just Red
        4 -> Just Orange
        5 -> Just Yellow
        6 -> Just Green
        7 -> Just Blue
        8 -> Just Indigo
        9 -> Just Violet
        _ -> Nothing

fromIntDefaultWhite : Int -> Colour
fromIntDefaultWhite int =
    case fromInt int of
        Just colour -> colour
        Nothing -> White

cycle : Colour -> Colour
cycle colour =
    colour
    |> toInt
    |> (+) 1
    |> modBy 10
    |> fromIntDefaultWhite

cycleMaybe : Maybe Colour -> Maybe Colour
cycleMaybe maybeColour =
    case maybeColour of
        Nothing ->
            Just White
        Just Violet ->
            Nothing
        Just c ->
            Just <| cycle c

encode colour =
    Encode.int (toInt colour)

decoder =
    Decode.map fromIntDefaultWhite Decode.int
