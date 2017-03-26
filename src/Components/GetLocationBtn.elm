module Components.GetLocationBtn exposing (..)

import Geolocation exposing (Error, Location)
import Task
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)


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
    | ReceivedReverseGeoLookup String
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
            , Cmd.none
            )

        ReceivedReverseGeoLookup address ->
            ( { model | loading = False }, Cmd.none )

        LocationFailed err ->
            ( { model
                | loadError = Just err
                , loading = False
              }
            , Cmd.none
            )



-- Reverse geo lookup
-- Views


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
