module FlickrImages exposing (..)

import Date
import Date.Distance as TimeAgo
import Http
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import Util exposing (parseHttpError, decodeDate, distanceInKm)


-- Model and init state


type alias Model =
    { apiKey : String
    , query : Query
    , results : List Image
    , loading : Bool
    , loadError : Maybe String
    }


type alias Image =
    { id : String
    , owner : String
    , ownerName : String
    , description : String
    , secret : String
    , server : String
    , farm : Int
    , title : String
    , date : Date.Date
    , longitude : String
    , latitude : String
    }


type alias Query =
    { currentPage : Int
    , totalPages : Int
    , perPage : Int
    , sort : String
    , radius : Int
    , radiusUnit : String
    , location : Location
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : String -> Model
initialState apiKey =
    { apiKey = apiKey
    , query =
        { currentPage = 0
        , totalPages = 0
        , perPage = 6
        , sort = "date-posted-dsc"
        , radius = 5
        , radiusUnit = "km"
        , location = Location "" 0 0
        }
    , results = []
    , loading = False
    , loadError = Nothing
    }



-- Update


type Msg
    = LoadImages Location
    | LoadNextPage
    | Reset
    | ImageSearchResponse (Result Http.Error (List Image))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadImages location ->
            let
                query =
                    model.query
            in
                ( { model
                    | loading = True
                    , results = []
                    , query = { query | location = location }
                  }
                , fetchImages model location model.apiKey
                )

        LoadNextPage ->
            let
                newModel =
                    if model.query.currentPage <= model.query.totalPages then
                        { model | loading = True }
                    else
                        model
            in
                ( newModel, Cmd.none )

        Reset ->
            ( initialState model.apiKey, Cmd.none )

        ImageSearchResponse response ->
            let
                newModel =
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
            in
                ( newModel, Cmd.none )



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


fetchImages : Model -> Location -> String -> Cmd Msg
fetchImages model location apiKey =
    let
        -- Docs: https://www.flickr.com/services/api/flickr.photos.search.html
        url =
            getFlickrApiUrl
                "?method=flickr.photos.search"
                [ ( "radius", toString model.query.radius )
                , ( "radius_units", model.query.radiusUnit )
                , ( "per_page", toString model.query.perPage )
                , ( "lat", toString location.lat )
                , ( "lon", toString location.lng )
                , ( "extras", "date_upload,geo,owner_name,description" )
                , ( "sort", model.query.sort )
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
        |> required "ownername" Decode.string
        |> requiredAt [ "description", "_content" ] Decode.string
        |> required "secret" Decode.string
        |> required "server" Decode.string
        |> required "farm" Decode.int
        |> required "title" Decode.string
        |> required "dateupload" decodeDate
        |> optional "longitude" Decode.string ""
        |> optional "latitude" Decode.string ""



-- Views


viewAuthor : String -> String -> Html Msg
viewAuthor name userId =
    div [ class "img-distance" ]
        [ span [ class "icon icon-user" ] []
        , a [ target "_blank", href <| "https://www.flickr.com/photos/" ++ userId ] [ text name ]
        ]


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
                    [ h3 [ class "h5 img-title" ] [ text imgData.title ]
                    , p [ class "img-desc" ] [ text imgData.description ]
                    , viewAuthor imgData.ownerName imgData.owner
                    , div [ class "img-date" ]
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
