module Components.GetLocationBtn exposing (..)

import Geolocation exposing (Error, Location)
import Task
import Http
import Json.Decode as Decode exposing (field, index)
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Util exposing (parseHttpError)


-- Model and init state


type alias Model =
    { loading : Bool
    , location : Maybe Geolocation.Location
    , loadError : Maybe String
    }


initialState : Model
initialState =
    { loading = False
    , location = Nothing
    , loadError = Nothing
    }



-- Update


type Msg
    = RequestLocation
    | ReceivedLocation Location
    | ReceivedReverseGeoLookup (Result Http.Error String)
    | LocationFailed String


onLocationResponse : Result Geolocation.Error Location -> Msg
onLocationResponse result =
    case result of
        Ok loc ->
            ReceivedLocation loc

        Err error ->
            case error of
                Geolocation.PermissionDenied err ->
                    LocationFailed "Permission denied"

                Geolocation.LocationUnavailable err ->
                    LocationFailed "Location unavailable"

                Geolocation.Timeout err ->
                    LocationFailed "Request timed out. Try again."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestLocation ->
            ( { model
                | loading = True
                , loadError = Nothing
                , location = Nothing
              }
            , Task.attempt onLocationResponse Geolocation.now
            )

        ReceivedLocation loc ->
            ( { model
                | location = Just loc
              }
            , doReverseGeoLookup loc
            )

        ReceivedReverseGeoLookup response ->
            let
                newModel =
                    case response of
                        Ok results ->
                            { model
                                | loading = False
                            }

                        Err error ->
                            { model
                                | loadError = Just (parseHttpError error)
                                , loading = False
                            }
            in
                ( newModel, Cmd.none )

        LocationFailed err ->
            ( { model
                | loadError = Just err
                , loading = False
              }
            , Cmd.none
            )



-- Reverse geo lookup
-- Views


doReverseGeoLookup : Location -> Cmd Msg
doReverseGeoLookup location =
    let
        url =
            "https://maps.googleapis.com/maps/api/geocode/json?latlng="
                ++ (String.join "," [ toString location.latitude, toString location.longitude ])
    in
        Http.get url geoLocationDecoder
            |> Http.send ReceivedReverseGeoLookup


geoLocationDecoder : Decode.Decoder String
geoLocationDecoder =
    field "results" <| index 0 (field "formatted_address" Decode.string)


viewGetLocationBtn : Model -> Html Msg
viewGetLocationBtn model =
    button
        [ class <|
            if model.loading then
                "get-location-btn loading"
            else
                "get-location-btn"
        , type_ "button"
        , onClick RequestLocation
        , disabled model.loading
        , title <|
            if model.loading then
                "Loading location..."
            else
                "Use my current location"
        ]
        []
