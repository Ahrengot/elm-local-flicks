module Update exposing (..)

import Msg exposing (..)
import Model exposing (..)
import Task
import Geolocation exposing (Location)
import Http


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
    let
        () =
            Debug.log "Got msg: " msg
    in
        case msg of
            RequestLocation ->
                ( { model
                    | loadingLocation = True
                    , locationError = Nothing
                  }
                , (Task.attempt onLocationResponse Geolocation.now)
                )

            ReceivedLocation loc ->
                ( { model
                    | location = Just loc
                    , loadingLocation = False
                  }
                , Cmd.none
                )

            LocationFailed err ->
                ( { model
                    | locationError = Just err
                    , loadingLocation = False
                  }
                , Cmd.none
                )

            RequestPlace ->
                ( { model
                    | loadingPlace = True
                    , placeLoadError = Nothing
                  }
                , Cmd.none
                )

            PlaceResponse response ->
                case response of
                    Ok result ->
                        ( { model
                            | loadingPlace = False
                            , place = Just result
                          }
                        , Cmd.none
                        )

                    Err error ->
                        let
                            errorMessage =
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
                        in
                            ( { model | placeLoadError = Just errorMessage, loadingPlace = False }, Cmd.none )
