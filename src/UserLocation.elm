module UserLocation exposing (..)

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
                    LocationFailed "Request timed out"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestLocation ->
            ( { model
                | loading = True
                , loadError = Nothing
              }
            , Task.attempt onLocationResponse Geolocation.now
            )

        ReceivedLocation loc ->
            ( { model
                | location = Just loc
                , loading = False
              }
            , Cmd.none
            )

        LocationFailed err ->
            ( { model
                | loadError = Just err
                , loading = False
              }
            , Cmd.none
            )



-- Views


viewGetLocationBtn : Model -> Html Msg
viewGetLocationBtn model =
    let
        btnText =
            if model.loading then
                "Loading..."
            else
                "Use my current location"
    in
        button
            [ class "btn btn-primary"
            , type_ "button"
            , onClick RequestLocation
            , disabled model.loading
            ]
            [ text btnText
            ]
