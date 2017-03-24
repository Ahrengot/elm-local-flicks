module FlickrImages exposing (..)

import Date
import Date.Distance as TimeAgo
import Http
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import Util exposing (parseHttpError, decodeDate, distanceInKm)


-- Model and init state


type alias Model =
    { results : List Image
    , loading : Bool
    , loadError : Maybe String
    }


type alias Image =
    { id : String
    , owner : String
    , secret : String
    , server : String
    , farm : Int
    , title : String
    , date : Date.Date
    , longitude : String
    , latitude : String
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : Model
initialState =
    { results = []
    , loading = False
    , loadError = Nothing
    }



-- Update


type Msg
    = ImageSearchResponse (Result Http.Error (List Image))


update : Msg -> Model -> Model
update msg model =
    case msg of
        ImageSearchResponse response ->
            case response of
                Ok results ->
                    { model
                        | loading = False
                        , results = results
                    }

                Err error ->
                    { model
                        | loadError = Just (parseHttpError error)
                        , loading = False
                    }



-- HTTP API interactions


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


fetchImages : Location -> String -> Cmd Msg
fetchImages location apiKey =
    let
        -- Docs: https://www.flickr.com/services/api/flickr.photos.search.html
        url =
            getFlickrApiUrl
                "?method=flickr.photos.search"
                [ ( "radius", "5" )
                , ( "radius_units", "km" )
                , ( "per_page", "25" )
                , ( "lat", toString location.lat )
                , ( "lon", toString location.lng )
                , ( "extras", "date_upload,geo" )
                , ( "sort", "date-posted-dsc" )
                , ( "format", "json" )
                , ( "nojsoncallback", "1" )
                , ( "api_key", apiKey )
                ]
    in
        Http.get url imageListDecoder
            |> Http.send ImageSearchResponse


imageListDecoder : Decode.Decoder (List Image)
imageListDecoder =
    Decode.at [ "photos", "photo" ] (Decode.list imageDecoder)


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



-- Views


viewImage : Maybe Location -> Float -> Image -> Html Msg
viewImage userLocation now imgData =
    let
        -- https://farm{farm-id}.staticflickr.com/{server-id}/{id}_{secret}_[mstzb].jpg
        srcBase =
            "https://farm" ++ (toString imgData.farm) ++ ".staticflickr.com/" ++ imgData.server ++ "/" ++ imgData.id ++ "_" ++ imgData.secret

        imgLocation =
            { lat = Result.withDefault 0 (String.toFloat imgData.latitude)
            , lon = Result.withDefault 0 (String.toFloat imgData.longitude)
            }

        distanceStr =
            if imgLocation.lat /= 0 && imgLocation.lon /= 0 then
                case userLocation of
                    Nothing ->
                        "Unavailable"

                    Just loc ->
                        distanceInKm loc.lng loc.lat imgLocation.lon imgLocation.lat
            else
                "Unavailable"
    in
        div [ class "grid-item" ]
            [ div [ class "img-card" ]
                [ a [ href (srcBase ++ "_b.jpg") ]
                    [ img [ Html.Attributes.src <| srcBase ++ "_z.jpg" ] []
                    ]
                , div [ class "img-meta" ]
                    [ div [ class "img-date" ]
                        [ span [ class "icon icon-date" ] []
                        , text <| (TimeAgo.inWords (Date.fromTime now) imgData.date) ++ " ago"
                        ]
                    , div [ class "img-distance" ]
                        [ span [ class "icon icon-location" ] []
                        , text distanceStr
                        ]
                    ]
                ]
            ]
