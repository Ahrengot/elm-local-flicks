port module Main exposing (..)

import Html
import Time
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Navigation
import Components.LocationAutocomplete as Autocomplete
import UserLocation
import FlickrImages
import Router


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
    , router : Router.Model
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : Flags -> Navigation.Location -> ( Model, Cmd Msg )
initialState flags location =
    ( { title = flags.title
      , flickrApiKey = flags.flickrApiKey
      , gmapsApiKey = flags.gmapsApiKey
      , now = 0
      , selectedLocation = Nothing
      , autocomplete = Autocomplete.initialState
      , userLocation = UserLocation.initialState
      , flickrImages = FlickrImages.initialState
      , router = Router.initialState location
      }
    , Task.perform UpdateTime Time.now
    )



-- Update and messages


port changeBodyBg : String -> Cmd msg


type Msg
    = UpdateTime Float
    | ClearLocation
    | SetLocation Location
    | AutocompleteMsg Autocomplete.Msg
    | UserLocationMsg UserLocation.Msg
    | FlickrMsg FlickrImages.Msg
    | RouterMsg Router.Msg
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime now ->
            ( { model | now = now }, Cmd.none )

        ClearLocation ->
            ( { model | selectedLocation = Nothing }, Cmd.none )

        SetLocation location ->
            let
                ( newFlickrImages, fCmd ) =
                    FlickrImages.update (FlickrImages.LoadImages location model.flickrApiKey) model.flickrImages
            in
                ( { model
                    | selectedLocation = Just location
                    , flickrImages = newFlickrImages
                  }
                , Cmd.map FlickrMsg fCmd
                )

        UrlChange location ->
            ( { model | router = Router.update (Router.UrlChange location) model.router }, Cmd.none )

        RouterMsg rMsg ->
            ( { model | router = Router.update rMsg model.router }, Cmd.none )

        AutocompleteMsg acMsg ->
            let
                ( newAutocomplete, acCmd ) =
                    Autocomplete.update acMsg model.autocomplete

                thisCmd =
                    case acMsg of
                        Autocomplete.AfterSelectItem location ->
                            Task.perform SetLocation <| Task.succeed <| location

                        _ ->
                            Cmd.none

                batchedCmd =
                    Cmd.batch [ thisCmd, Cmd.map AutocompleteMsg acCmd ]
            in
                ( { model | autocomplete = newAutocomplete }, batchedCmd )

        UserLocationMsg locMsg ->
            let
                ( newUserLocation, locCmd ) =
                    UserLocation.update locMsg model.userLocation

                thisCmd =
                    case locMsg of
                        UserLocation.RequestLocation ->
                            Task.perform (\_ -> ClearLocation) <| Task.succeed Nothing

                        UserLocation.ReceivedLocation loc ->
                            Task.perform SetLocation <| Task.succeed <| Location "My current location" loc.latitude loc.longitude

                        _ ->
                            Cmd.none

                batchedCmd =
                    Cmd.batch [ thisCmd, Cmd.map UserLocationMsg locCmd ]
            in
                ( { model | userLocation = newUserLocation }, batchedCmd )

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

        route =
            case model.router.route of
                Nothing ->
                    "404 Not found"

                Just route ->
                    case route of
                        Router.Home ->
                            "Home"

                        Router.LocationSearch location ->
                            "Location search: '" ++ location ++ "'"
    in
        div [ class "app-container" ]
            [ div []
                [ header [ class "header" ]
                    [ h1 [] [ text model.title ]
                    , p [ class "app-desc" ] [ text "Search for Flickr images posted around The World" ]
                    , p [ class "app-desc" ] [ text <| "Current route: " ++ route ]
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
