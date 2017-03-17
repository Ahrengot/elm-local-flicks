port module Update exposing (..)

import Msg exposing (..)
import Model exposing (..)
import Command exposing (fetchImages)
import Task
import Geolocation exposing (Location)
import Http


port changeBodyBg : String -> Cmd msg


parseHttpError : Http.Error -> String
parseHttpError error =
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
                | loadingLocation = True
                , locationLoadError = Nothing
              }
            , (Task.attempt onLocationResponse Geolocation.now)
            )

        ReceivedTime now ->
            ( { model | now = now }, Cmd.none )

        ReceivedLocation loc ->
            ( { model
                | location = Just loc
                , loadingLocation = False
                , loadingPlace = True
              }
            , (fetchImages loc model.apiKey)
            )

        LocationFailed err ->
            ( { model
                | locationLoadError = Just err
                , loadingLocation = False
              }
            , Cmd.none
            )

        ImagesResponse response ->
            case response of
                Ok results ->
                    ( { model
                        | loadingImages = False
                        , images = results
                      }
                    , changeBodyBg "#151515"
                    )

                Err error ->
                    ( { model
                        | imagesLoadError = Just (parseHttpError error)
                        , loadingImages = False
                      }
                    , Cmd.none
                    )
