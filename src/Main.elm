port module Main exposing (..)

import Html
import Time
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Http exposing (decodeUri)
import Navigation
import Components.LocationAutocomplete as Autocomplete
import Components.GetLocationBtn as GetLocationBtn
import FlickrImages
import Router
import Suggestions exposing (viewSuggestionLinks)


-- Initial model and state


type alias Flags =
    { title : String
    , flickrApiKey : String
    }


type alias Model =
    { title : String
    , now : Float
    , selectedLocation : Maybe Location
    , autocomplete : Autocomplete.Model
    , userLocation : GetLocationBtn.Model
    , flickrImages : FlickrImages.Model
    , router : Router.Model
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : Flags -> Navigation.Location -> ( Model, Cmd Msg )
initialState flags location =
    let
        init =
            { title = flags.title
            , now = 0
            , selectedLocation = Nothing
            , autocomplete = Autocomplete.initialState
            , userLocation = GetLocationBtn.initialState
            , flickrImages = FlickrImages.initialState flags.flickrApiKey
            , router = Router.initialState location
            }

        -- Make any model/cmd modifications we need to
        -- based on initial url location
        ( model, cmd ) =
            modelFromUrlLocation location init

        initCmd =
            Task.perform UpdateTime Time.now
    in
        ( model, Cmd.batch [ cmd, initCmd ] )



-- Update and messages


port changeBodyBg : String -> Cmd msg


type Msg
    = UpdateTime Float
    | AutocompleteMsg Autocomplete.Msg
    | UserLocationMsg GetLocationBtn.Msg
    | FlickrMsg FlickrImages.Msg
    | UrlChange Navigation.Location
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateTime now ->
            ( { model | now = now }, Cmd.none )

        UrlChange urlLocation ->
            modelFromUrlLocation urlLocation model

        AutocompleteMsg acMsg ->
            let
                ( newAutocomplete, acCmd ) =
                    Autocomplete.update acMsg model.autocomplete

                thisCmd =
                    case acMsg of
                        Autocomplete.AfterSelectItem location ->
                            locationUrlCmd model location

                        _ ->
                            Cmd.none

                batchedCmd =
                    Cmd.batch [ thisCmd, Cmd.map AutocompleteMsg acCmd ]
            in
                ( { model | autocomplete = newAutocomplete }, batchedCmd )

        UserLocationMsg locMsg ->
            let
                ( newUserLocation, locCmd ) =
                    GetLocationBtn.update locMsg model.userLocation

                ( newAutocomplete, thisCmd ) =
                    case locMsg of
                        GetLocationBtn.RequestLocation ->
                            ( Tuple.first <|
                                Autocomplete.update (Autocomplete.SetDefaultQuery "Loading your location...") model.autocomplete
                            , Cmd.none
                            )

                        GetLocationBtn.ReceivedReverseGeoLookup response ->
                            case response of
                                Ok address ->
                                    case newUserLocation.location of
                                        Just loc ->
                                            ( model.autocomplete, locationUrlCmd model <| Location address loc.latitude loc.longitude )

                                        Nothing ->
                                            ( model.autocomplete, Cmd.none )

                                Err _ ->
                                    ( model.autocomplete, Cmd.none )

                        _ ->
                            ( model.autocomplete, Cmd.none )

                batchedCmd =
                    Cmd.batch [ thisCmd, Cmd.map UserLocationMsg locCmd ]
            in
                ( { model
                    | userLocation = newUserLocation
                    , autocomplete = newAutocomplete
                  }
                , batchedCmd
                )

        FlickrMsg fMsg ->
            let
                ( newFlickrImages, fCmd ) =
                    FlickrImages.update fMsg model.flickrImages

                cmd =
                    case fMsg of
                        -- Change body bg if we have image results
                        FlickrImages.ImageSearchResponse res ->
                            case res of
                                Ok results ->
                                    changeBodyBg "#151515"

                                Err error ->
                                    changeBodyBg ""

                        _ ->
                            Cmd.none

                batchedCmd =
                    Cmd.batch [ cmd, Cmd.map FlickrMsg fCmd ]
            in
                ( { model | flickrImages = newFlickrImages }, batchedCmd )



-- Utility functions


locationUrlCmd : Model -> Location -> Cmd Msg
locationUrlCmd model placeLocation =
    case List.head model.router.history of
        Just urlLocation ->
            Navigation.newUrl <| Router.locationToHash placeLocation

        Nothing ->
            Cmd.none


modelFromUrlLocation : Navigation.Location -> Model -> ( Model, Cmd Msg )
modelFromUrlLocation urlLocation model =
    let
        newRouter =
            Router.update (Router.UrlChange urlLocation) model.router

        ( newSelectedLocation, newFlickrImages, newAutocomplete, cmd ) =
            case newRouter.route of
                Router.LocationSearch locationName ( lat, lng ) ->
                    let
                        location =
                            Location locationName lat lng

                        ( newFlickrImages, fCmd ) =
                            FlickrImages.update (FlickrImages.LoadImages location) model.flickrImages

                        ( newAutocomplete, _ ) =
                            Autocomplete.update
                                (Autocomplete.SetDefaultQuery <|
                                    Maybe.withDefault "unknown location" <|
                                        decodeUri locationName
                                )
                                model.autocomplete
                    in
                        ( Just location, newFlickrImages, newAutocomplete, Cmd.map FlickrMsg fCmd )

                Router.Home ->
                    let
                        ( newAutocomplete, _ ) =
                            Autocomplete.update Autocomplete.HandleEscape model.autocomplete
                                |> Tuple.first
                                |> Autocomplete.update (Autocomplete.SetDefaultQuery "")

                        ( newFlickrImages, _ ) =
                            FlickrImages.update FlickrImages.Reset model.flickrImages
                    in
                        ( Nothing, newFlickrImages, newAutocomplete, changeBodyBg "" )

                _ ->
                    ( Nothing, model.flickrImages, model.autocomplete, Cmd.none )
    in
        ( { model
            | router = newRouter
            , selectedLocation = newSelectedLocation
            , flickrImages = newFlickrImages
            , autocomplete = newAutocomplete
          }
        , cmd
        )



-- Views


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = initialState
        , view = viewApp
        , update = update
        , subscriptions = subscriptions
        }


viewApp : Model -> Html Msg
viewApp model =
    let
        errorView =
            [ model.userLocation.loadError, model.flickrImages.loadError ]
                |> List.map
                    (\maybeErr ->
                        case maybeErr of
                            Nothing ->
                                text ""

                            Just errorText ->
                                viewError errorText
                    )
                |> div [ class "errors" ]

        headerContents =
            case model.router.route of
                Router.NotFound ->
                    []

                _ ->
                    [ p [ class "app-desc" ] [ text "Flickr images from around The World" ]
                    , div [ class "autocomplete-wrap" ]
                        [ Html.map AutocompleteMsg <| lazy Autocomplete.view model.autocomplete
                        , Html.map UserLocationMsg <| lazy GetLocationBtn.viewGetLocationBtn model.userLocation
                        , a
                            [ href "#"
                            , class "autocomplete-input-reset"
                            , title "Clear search"
                            , style
                                [ ( "display"
                                  , if model.autocomplete.query == "" then
                                        "none"
                                    else
                                        ""
                                  )
                                ]
                            ]
                            [ text "×" ]
                        ]
                    ]

        viewContents =
            case model.router.route of
                Router.Home ->
                    [ errorView
                    , div [ class "location-suggestions" ] <|
                        List.map (\el -> Html.map (\a -> NoOp) el) viewSuggestionLinks
                    ]

                Router.LocationSearch locationName ( lat, lng ) ->
                    [ errorView
                    , Html.map FlickrMsg <| lazy viewImageGrid model
                    ]

                Router.NotFound ->
                    [ p [ class "app-desc text-center" ]
                        [ text "Route not found. "
                        , a [ href "./" ] [ text "Go to home page." ]
                        ]
                    ]
    in
        div [ class "app-container" ]
            [ div []
                [ header [ class "header" ]
                    [ h1 []
                        [ text model.title ]
                    , div [] headerContents
                    ]
                , div [] viewContents
                ]
            ]


viewError : String -> Html Msg
viewError msg =
    div [ class "alert alert-danger mt-3 mb-3" ]
        [ p [ class "mb-0" ] [ text msg ] ]


viewImageGrid : Model -> Html FlickrImages.Msg
viewImageGrid model =
    if model.flickrImages.loading then
        div [ class "image-grid-load-indicator" ] [ text "Loading images..." ]
    else if model.flickrImages.loading == False && List.isEmpty model.flickrImages.results then
        div [ class "image-grid-load-indicator" ] [ text "No images found at this location. Try somewhere else.." ]
    else
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
