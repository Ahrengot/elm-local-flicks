port module Main exposing (..)

import Html
import Time
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.LocationAutocomplete as Autocomplete
import UserLocation
import FlickrImages
import Html.Lazy exposing (lazy)


-- Initial model and state


type alias Flags =
    { title : String
    , flickrApiKey : String
    , gmapsApiKey : String
    }


type alias Model =
    { title : String
    , flickrApiKey : String
    , gmapsApiKey : String
    , now : Float
    , selectedLocation : Maybe Location
    , autocomplete : Autocomplete.Model
    , userLocation : UserLocation.Model
    , flickrImages : FlickrImages.Model
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : Flags -> ( Model, Cmd Msg )
initialState flags =
    ( { title = flags.title
      , flickrApiKey = flags.flickrApiKey
      , gmapsApiKey = flags.gmapsApiKey
      , now = 0
      , selectedLocation = Nothing
      , autocomplete = Autocomplete.initialState
      , userLocation = UserLocation.initialState
      , flickrImages = FlickrImages.initialState
      }
    , Task.perform UpdateTime Time.now
    )



-- Update and messages


port changeBodyBg : String -> Cmd msg


type Msg
    = UpdateTime Float
    | AutocompleteMsg Autocomplete.Msg
    | UserLocationMsg UserLocation.Msg
    | FlickrMsg FlickrImages.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime now ->
            ( { model | now = now }, Cmd.none )

        AutocompleteMsg acMsg ->
            let
                ( newAutocomplete, acCmd ) =
                    Autocomplete.update acMsg model.autocomplete

                ( newModel, thisCmd ) =
                    case acMsg of
                        Autocomplete.AfterSelectItem location ->
                            let
                                ( newFlickrImages, fCmd ) =
                                    FlickrImages.update (FlickrImages.LoadImages location model.flickrApiKey) model.flickrImages
                            in
                                ( { model
                                    | autocomplete = newAutocomplete
                                    , selectedLocation = Just location
                                    , flickrImages = newFlickrImages
                                  }
                                , Cmd.map FlickrMsg fCmd
                                )

                        _ ->
                            ( { model | autocomplete = newAutocomplete }, Cmd.none )

                cmd =
                    Cmd.map AutocompleteMsg acCmd
            in
                ( newModel, Cmd.batch [ cmd, thisCmd ] )

        UserLocationMsg locMsg ->
            let
                ( newUserLocation, locCmd ) =
                    UserLocation.update locMsg model.userLocation

                location =
                    case locMsg of
                        UserLocation.ReceivedLocation loc ->
                            Just <| Location "My current location" loc.latitude loc.longitude

                        _ ->
                            Nothing

                ( newModel, cmd ) =
                    case location of
                        Nothing ->
                            ( { model | userLocation = newUserLocation }, Cmd.map UserLocationMsg locCmd )

                        Just location ->
                            let
                                ( newFlickrImages, fCmd ) =
                                    FlickrImages.update (FlickrImages.LoadImages location model.flickrApiKey) model.flickrImages
                            in
                                ( { model
                                    | userLocation = newUserLocation
                                    , flickrImages = newFlickrImages
                                  }
                                , Cmd.map FlickrMsg fCmd
                                )
            in
                ( newModel, cmd )

        FlickrMsg fMsg ->
            let
                ( newFlickrImages, fCmd ) =
                    FlickrImages.update fMsg model.flickrImages
            in
                ( { model
                    | flickrImages = newFlickrImages
                  }
                , case fMsg of
                    -- Change body bg if we have image results
                    FlickrImages.ImageSearchResponse res ->
                        case res of
                            Ok results ->
                                changeBodyBg "#151515"

                            Err error ->
                                changeBodyBg ""

                    _ ->
                        Cmd.none
                )



-- Views


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initialState
        , view = viewApp
        , update = update
        , subscriptions = subscriptions
        }


viewApp : Model -> Html Msg
viewApp model =
    let
        resultsText =
            if (List.length model.flickrImages.results) > 0 then
                p [ class "results-description" ] [ text ("Found " ++ (toString (List.length model.flickrImages.results)) ++ " images.") ]
            else
                text ""

        errors =
            [ model.userLocation.loadError, model.flickrImages.loadError ]
                |> List.map
                    (\maybeErr ->
                        case maybeErr of
                            Nothing ->
                                text ""

                            Just errorText ->
                                viewError errorText
                    )
    in
        div [ class "app-container" ]
            [ div []
                [ header [ class "header" ]
                    [ h1 [] [ text model.title ]
                    , p [ class "app-desc" ] [ text "Search for Flickr images posted around The World" ]
                    , Html.map AutocompleteMsg <| lazy Autocomplete.view model.autocomplete
                    , div [ class "btn-group" ]
                        [ Html.map UserLocationMsg <| lazy UserLocation.viewGetLocationBtn model.userLocation
                        ]
                    ]
                , div [ class "errors" ] errors
                , Html.map FlickrMsg <| lazy viewImageGrid model
                ]
            ]


viewError : String -> Html Msg
viewError msg =
    div [ class "alert alert-danger mt-3 mb-3" ]
        [ p [ class "mb-0" ] [ text msg ] ]


viewImageGrid : Model -> Html FlickrImages.Msg
viewImageGrid model =
    model.flickrImages.results
        |> List.map (FlickrImages.viewImage model.selectedLocation model.now)
        |> div [ class "image-grid" ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ (Time.every Time.minute UpdateTime)
        , Sub.map AutocompleteMsg <| Autocomplete.subscriptions model.autocomplete
        ]
