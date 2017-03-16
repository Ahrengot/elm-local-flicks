module Command exposing (..)

import Model exposing (Place, Image)
import Msg exposing (..)
import Geolocation exposing (Location)
import Http
import Date
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)


getFlickrApiUrl : String -> List ( String, String ) -> String
getFlickrApiUrl endpoint args =
    -- Example URL: https://api.flickr.com/services/rest/?method=flickr.places.findByLatLon&api_key=02ff0bcdae6bb7b4929cc30b56b9f4ce&lat=10&lon=20&format=json&nojsoncallback=1
    let
        queryString =
            List.foldl
                (\( key, value ) memo -> memo ++ "&" ++ key ++ "=" ++ value)
                ""
                args
    in
        "https://api.flickr.com/services/rest/" ++ endpoint ++ queryString


fetchPlace : Location -> String -> Cmd Msg
fetchPlace location apiKey =
    let
        url =
            getFlickrApiUrl
                "?method=flickr.places.findByLatLon"
                [ ( "lat", toString location.latitude )
                , ( "lon", toString location.longitude )
                , ( "format", "json" )
                , ( "nojsoncallback", "1" )
                , ( "api_key", apiKey )
                ]
    in
        Http.get url placeListDecoder
            |> Http.send PlaceResponse


fetchImages : Place -> String -> Cmd Msg
fetchImages place apiKey =
    let
        -- Docs: https://www.flickr.com/services/api/flickr.photos.search.html
        url =
            getFlickrApiUrl
                "?method=flickr.photos.search"
                [ ( "radius", "10" )
                , ( "radius_units", "km" )
                , ( "per_page", "25" )
                , ( "place_id", place.place_id )
                , ( "extras", "date_upload,geo" )
                , ( "sort", "date-posted-dsc" )
                , ( "format", "json" )
                , ( "nojsoncallback", "1" )
                , ( "api_key", apiKey )
                ]
    in
        Http.get url imageListDecoder
            |> Http.send ImagesResponse


placeListDecoder : Decode.Decoder (List Place)
placeListDecoder =
    Decode.at [ "places", "place" ] (Decode.list placeDecoder)


placeDecoder : Decode.Decoder Place
placeDecoder =
    decode Place
        |> required "place_id" Decode.string
        |> required "woeid" Decode.string
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string
        |> required "place_url" Decode.string
        |> required "place_type" Decode.string
        |> required "place_type_id" Decode.string
        |> required "name" Decode.string
        |> required "woe_name" Decode.string


imageListDecoder : Decode.Decoder (List Image)
imageListDecoder =
    Decode.at [ "photos", "photo" ] (Decode.list imageDecoder)


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


imageDecoder : Decode.Decoder Image
imageDecoder =
    decode Image
        |> required "id" Decode.string
        |> required "owner" Decode.string
        |> required "secret" Decode.string
        |> required "server" Decode.string
        |> required "farm" Decode.int
        |> required "title" Decode.string
        |> required "dateupload" decodeDate
        |> optional "longitude" Decode.string ""
        |> optional "latitude" Decode.string ""
