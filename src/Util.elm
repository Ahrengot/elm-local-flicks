module Util exposing (..)

import Round
import Http
import Date
import Regex exposing (regex)
import Json.Decode as Decode
import Html exposing (Html, span, strong, text)


parseHttpError : Http.Error -> String
parseHttpError error =
    case error of
        Http.Timeout ->
            "Server response timed out. Please try again."

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error. Check your internet connection and try again."

        Http.BadStatus res ->
            "The server responded with an error: " ++ res.status.message

        Http.BadPayload err res ->
            "Error parsing JSON: " ++ err



{-
   distanceInKm:
    Uses the Haversine formula to calculate the distance
    between two sets of Lat/Lon coordinates
-}


distanceInKm : Float -> Float -> Float -> Float -> String
distanceInKm lon1 lat1 lon2 lat2 =
    let
        -- Radius of The Earth in km
        earthRadius =
            6371

        dLat =
            degrees (lat2 - lat1)

        dLon =
            degrees (lon2 - lon1)

        haversine =
            (sin <| dLat / 2)
                * (sin <| dLat / 2)
                + (cos <| degrees lat1)
                * (cos <| degrees lat2)
                * (sin <| dLon / 2)
                * (sin <| dLon / 2)

        c =
            2 * (atan2 (sqrt haversine) (sqrt 1 - haversine))
    in
        (Round.round 2 (earthRadius * c)) ++ " km"



{-
   decodeDate:
    Converts timestamps (seconds, NOT ms, since Unix epoc) to Elm Date
    This is specific to Flickr since most timestamps are in ms rather
    than seconds
-}


decodeDate : Decode.Decoder Date.Date
decodeDate =
    Decode.string
        |> Decode.andThen
            (\val ->
                case String.toFloat val of
                    Err err ->
                        Decode.fail err

                    Ok ms ->
                        -- (ms * 1000) because timestamp is in seconds, not miliseconds
                        Decode.succeed (Date.fromTime (ms * 1000))
            )



{-
   highlightMatches

    Wraps matches in `<strong>` elements and returns a list of Html
    If no matches are found then return a list with a single child
    containing the original string wrapped in a `<span>`
-}


highlightMatches : String -> String -> List (Html Never)
highlightMatches needle haystack =
    let
        needleLen =
            String.length needle

        haystackLen =
            String.length haystack

        lcNeedle =
            String.toLower needle

        lcHaystack =
            String.toLower haystack

        matches : List Regex.Match
        matches =
            Regex.find Regex.All (regex lcNeedle) lcHaystack

        matchByNumber : Int -> Maybe Regex.Match
        matchByNumber num =
            List.head <| List.filter (\match -> match.number == num) matches

        highlightMatch : Regex.Match -> Html Never
        highlightMatch match =
            let
                before =
                    if match.index == 0 then
                        ""
                    else
                        case matchByNumber (match.number - 1) of
                            Nothing ->
                                String.slice 0 match.index haystack

                            Just prevMatch ->
                                String.slice (prevMatch.index + 1) match.index haystack

                after =
                    case (List.head <| List.reverse matches) of
                        Nothing ->
                            ""

                        Just lastMatch ->
                            -- Check if this match is the last match we found
                            if lastMatch.number == match.number then
                                -- Check if last match is also last char in string. If not
                                -- then add the remaining characters
                                if (lastMatch.index + needleLen) == haystackLen then
                                    ""
                                else
                                    String.slice (lastMatch.index + needleLen) haystackLen haystack
                            else
                                ""
            in
                span []
                    [ text before
                    , strong [] [ text <| String.slice match.index (match.index + needleLen) haystack ]
                    , text after
                    ]
    in
        if List.isEmpty matches then
            [ span [] [ text haystack ] ]
        else
            List.map highlightMatch matches
