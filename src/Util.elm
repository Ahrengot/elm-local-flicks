module Util exposing (..)

import Round


type alias LatLng =
    { lat : Float, lon : Float }


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
