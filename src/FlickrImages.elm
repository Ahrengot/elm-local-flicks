module FlickrImages exposing (..)

import Date
import Date.Distance as TimeAgo
import Http
import Html exposing (..)
import Html.Attributes exposing (class, for, class, selected, value, id, target, href, rel)
import Html.Events as E
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
    , totalImages : Int
    , perPage : Int
    , sort : SortValue
    , radius : Int
    , radiusUnit : String
    , location : Location
    }


type SortValue
    = DatePostedAsc
    | DatePostedDesc
    | DateTakenAsc
    | DateTakenDesc
    | InterestingnessDesc
    | InterestingnessAsc


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


type alias SearchResults =
    { page : Int
    , pages : Int
    , perpage : Int
    , results : List Image
    , total : Int
    }


initialState : String -> Model
initialState apiKey =
    { apiKey = apiKey
    , query =
        { currentPage = 0
        , totalPages = 0
        , totalImages = 0
        , perPage = 15
        , sort = DatePostedDesc
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
    | ChangeSort SortValue
    | LoadNextPage
    | Reset
    | ImageSearchResponse (Result Http.Error SearchResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialState model.apiKey, Cmd.none )

        ChangeSort sortValue ->
            let
                query =
                    model.query

                newQuery =
                    { query | sort = sortValue }

                defaultState =
                    initialState model.apiKey

                ( newModel, newCmd ) =
                    { defaultState | query = newQuery } |> update (LoadImages query.location)
            in
                ( newModel, newCmd )

        LoadImages location ->
            let
                query =
                    model.query

                newModel =
                    { model
                        | loading = True
                        , results = []
                        , query =
                            { query
                                | location = location
                                , totalPages = 0
                                , currentPage = 1
                                , totalImages = 0
                            }
                    }
            in
                ( newModel, fetchImages newModel location newModel.apiKey )

        LoadNextPage ->
            let
                query =
                    model.query

                newModel =
                    if model.query.currentPage <= model.query.totalPages then
                        { model
                            | loading = True
                            , query = { query | currentPage = query.currentPage + 1 }
                        }
                    else
                        model
            in
                ( newModel, fetchImages newModel newModel.query.location newModel.apiKey )

        ImageSearchResponse response ->
            let
                query =
                    model.query

                newQuery =
                    case response of
                        Ok result ->
                            { query
                                | currentPage = result.page
                                , totalPages = result.pages
                                , totalImages = result.total
                            }

                        Err error ->
                            model.query

                newResults =
                    case response of
                        -- If first page just use results
                        -- otherwise append new results to exisiting list
                        Ok result ->
                            if newQuery.currentPage == 1 then
                                result.results
                            else
                                List.append model.results result.results

                        Err error ->
                            model.results

                newModel =
                    case response of
                        Ok result ->
                            { model
                                | loading = False
                                , results = newResults
                                , query = newQuery
                            }

                        Err error ->
                            { model
                                | loadError = Just (parseHttpError error)
                                , loading = False
                            }
            in
                ( newModel, Cmd.none )



-- HTTP API interactions


parseSortType : SortValue -> { label : String, value : String }
parseSortType sortValue =
    case sortValue of
        DatePostedAsc ->
            { label = "Date posted - old", value = "date-posted-asc" }

        DatePostedDesc ->
            { label = "Date posted - new", value = "date-posted-desc" }

        DateTakenAsc ->
            { label = "Date taken - old", value = "date-taken-asc" }

        DateTakenDesc ->
            { label = "Date taken - new", value = "date-taken-desc" }

        InterestingnessAsc ->
            { label = "Interesting - least", value = "interestingness-asc" }

        InterestingnessDesc ->
            { label = "Interesting - most", value = "interestingness-desc" }


sortStringToType : String -> SortValue
sortStringToType str =
    case str of
        "date-posted-asc" ->
            DatePostedAsc

        "date-posted-desc" ->
            DatePostedDesc

        "date-taken-asc" ->
            DateTakenAsc

        "date-taken-desc" ->
            DateTakenDesc

        "interestingness-desc" ->
            InterestingnessDesc

        "interestingness-asc" ->
            InterestingnessAsc

        _ ->
            DatePostedAsc


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
                , ( "page", toString model.query.currentPage )
                , ( "per_page", toString model.query.perPage )
                , ( "lat", toString location.lat )
                , ( "lon", toString location.lng )
                , ( "extras", "date_upload,geo,owner_name,description" )
                , ( "sort", (parseSortType model.query.sort).value )
                , ( "format", "json" )
                , ( "nojsoncallback", "1" )
                , ( "api_key", apiKey )
                ]
    in
        Http.get url imageListDecoder
            |> Http.send ImageSearchResponse


stringIntDecoder : Decode.Decoder Int
stringIntDecoder =
    Decode.map (\str -> String.toInt (str) |> Result.withDefault 0) Decode.string


imageListDecoder : Decode.Decoder SearchResults
imageListDecoder =
    decode SearchResults
        |> requiredAt [ "photos", "page" ] Decode.int
        |> requiredAt [ "photos", "pages" ] Decode.int
        |> requiredAt [ "photos", "perpage" ] Decode.int
        |> requiredAt [ "photos", "photo" ] (Decode.list imageDecoder)
        |> requiredAt [ "photos", "total" ] stringIntDecoder


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


onChangeSort : (String -> Msg) -> Attribute Msg
onChangeSort msg =
    E.on "change" (Decode.map msg E.targetValue)


viewSortSelect : Model -> Html Msg
viewSortSelect model =
    let
        options =
            [ DatePostedAsc, DatePostedDesc, InterestingnessDesc, InterestingnessAsc, DateTakenAsc, DateTakenDesc ]
                |> List.map
                    (\sortType ->
                        let
                            parsed =
                                parseSortType sortType

                            isSelected =
                                (sortType == model.query.sort)
                        in
                            option [ selected isSelected, value parsed.value ] [ text parsed.label ]
                    )
    in
        div [ class "sort-dropdown" ]
            [ label [ for "sort" ] [ text "Sort by" ]
            , select [ id "sort", onChangeSort (\str -> ChangeSort (sortStringToType str)) ] options
            ]


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

        imgFlickrUrl =
            "https://www.flickr.com/photos/" ++ imgData.owner ++ "/" ++ imgData.id

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
                [ a [ href imgFlickrUrl, rel "nofollow" ]
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
