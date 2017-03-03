module Command exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optionalAt)


getFlickrApiUrl : String -> List ( String, String ) -> String
getFlickrApiUrl endpoint args =
    -- Example URL: https://api.flickr.com/services/rest/?method=flickr.places.findByLatLon&api_key=02ff0bcdae6bb7b4929cc30b56b9f4ce&lat=10&lon=20&format=json&nojsoncallback=1
    let
        queryString =
            List.foldl
                (\( key, value ) memo -> "&" ++ key ++ "=" ++ value)
                ""
                args
    in
        "https://api.flickr.com/services/rest/" ++ endpoint ++ queryString


fetchPlace : Model -> Cmd Msg
fetchPlace model =
    let
        url =
            case model.location of
                Nothing ->
                    ""

                Just loc ->
                    getFlickrApiUrl
                        "?method=flickr.places.findByLatLon"
                        [ ( "lat", toString loc.latitude )
                        , ( "lon", toString loc.longitude )
                        , ( "format", "json" )
                        , ( "nojsoncallback", "1" )
                        ]
    in
        Http.get url placeDecoder
            |> Http.send RequestPlace


placeDecoder : Decode.Decoder Place
placeDecoder =
    Decode.at [ "places", "place" ]
        (decode Place
            |> required "place_id" Decode.string
            |> required "woeid" Decode.string
            |> required "latitude" Decode.string
            |> required "longitude" Decode.string
            |> required "place_url" Decode.string
            |> required "place_type" Decode.string
            |> required "place_type_id" Decode.string
            |> required "name" Decode.string
            |> required "woe_name" Decode.string
        )
