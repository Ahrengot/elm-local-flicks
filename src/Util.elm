module Util exposing (..)

import Round
import Http
import Date
import Json.Decode as Decode


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
