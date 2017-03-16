module Update exposing (..)

import Msg exposing (..)
import Model exposing (..)
import Command exposing (fetchPlace, fetchImages)
import Task
import Geolocation exposing (Location)
import Http


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
            , (fetchPlace loc model.apiKey)
            )

        LocationFailed err ->
            ( { model
                | locationLoadError = Just err
                , loadingLocation = False
              }
            , Cmd.none
            )

        PlaceResponse response ->
            case response of
                Ok results ->
                    let
                        msg =
                            case (List.head results) of
                                Just place ->
                                    (fetchImages place model.apiKey)

                                Nothing ->
                                    Cmd.none
                    in
                        ( { model
                            | loadingPlace = False
                            , place = List.head results
                            , loadingImages = True
                          }
                        , msg
                        )

                Err error ->
                    ( { model | placeLoadError = Just (parseHttpError error), loadingPlace = False }, Cmd.none )

        ImagesResponse response ->
            case response of
                Ok results ->
                    ( { model
                        | loadingImages = False
                        , images = results
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | imagesLoadError = Just (parseHttpError error)
                        , loadingImages = False
                      }
                    , Cmd.none
                    )
