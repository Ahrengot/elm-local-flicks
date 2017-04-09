module Suggestions exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (decodeUri)
import Regex


suggestions : List String
suggestions =
    [ "#/location/Nuuk/@/64.18140989999999,-51.694138"
    , "#/location/Sofia/@/42.6977082,23.3218675"
    , "#/location/Dubai/@/25.2048493,55.2707828"
    , "#/location/Brooklyn/@/40.6781784,-73.9441579"
    , "#/location/S%C3%A3o%20Paulo/@/-23.5505199,-46.63330939999999"
    ]


nameFromUrl url =
    let
        firstSubMatch : String -> List (Maybe String) -> String
        firstSubMatch fallback submatches =
            case List.head submatches of
                Nothing ->
                    fallback

                Just maybeString ->
                    Maybe.withDefault fallback maybeString

        firstMatch =
            List.head <| Regex.find (Regex.AtMost 1) (Regex.regex "^#/location/(.[^/]+)") url

        linkLabel =
            case firstMatch of
                Nothing ->
                    url

                Just match ->
                    firstSubMatch url match.submatches
    in
        li [] [ a [ href url ] [ text <| Maybe.withDefault url <| decodeUri linkLabel ] ]


viewSuggestionLinks : List (Html Never)
viewSuggestionLinks =
    let
        links =
            List.map nameFromUrl suggestions
    in
        if List.isEmpty links then
            []
        else
            (text "Or try one of these: ") :: [ ul [] links ]
