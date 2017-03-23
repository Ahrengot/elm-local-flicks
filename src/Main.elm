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
    , autocomplete : Autocomplete.Model
    , location : UserLocation.Model
    , flickrImages : FlickrImages.Model
    }


initialState : Flags -> ( Model, Cmd Msg )
initialState flags =
    ( { title = flags.title
      , flickrApiKey = flags.flickrApiKey
      , gmapsApiKey = flags.gmapsApiKey
      , now = 0
      , autocomplete = Autocomplete.initialState
      , location = UserLocation.initialState
      , flickrImages = FlickrImages.initialState
      }
    , Task.perform UpdateTime Time.now
    )



-- Update and messages


port changeBodyBg : String -> Cmd msg


type Msg
    = UpdateTime Float
    | AutocompleteMsg Autocomplete.Msg
    | LocationMsg UserLocation.Msg
    | FlickrMsg FlickrImages.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTime now ->
            ( { model
                | now = now
              }
            , Cmd.none
            )

        AutocompleteMsg acMsg ->
            let
                ( acState, acCmd ) =
                    Autocomplete.update acMsg model.autocomplete
            in
                ( { model | autocomplete = acState }, Cmd.map AutocompleteMsg acCmd )

        LocationMsg locMsg ->
            let
                ( locationState, locCmd ) =
                    UserLocation.update locMsg model.location

                thisCmd =
                    case locMsg of
                        UserLocation.ReceivedLocation loc ->
                            Cmd.map FlickrMsg <| FlickrImages.fetchImages loc model.flickrApiKey

                        _ ->
                            Cmd.none

                finalCmd =
                    Cmd.batch [ thisCmd, (Cmd.map LocationMsg locCmd) ]
            in
                ( { model
                    | location = locationState
                  }
                , finalCmd
                )

        FlickrMsg fMsg ->
            ( { model
                | flickrImages = FlickrImages.update fMsg model.flickrImages
              }
            , case fMsg of
                -- Change body bg if we have image results
                FlickrImages.ImageSearchResponse res ->
                    case res of
                        Ok results ->
                            changeBodyBg "#151515"

                        Err error ->
                            changeBodyBg ""
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
            [ model.location.loadError, model.flickrImages.loadError ]
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
                        [ Html.map LocationMsg <| lazy UserLocation.viewGetLocationBtn model.location
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
        |> List.map (FlickrImages.viewImage model.location.location model.now)
        |> div [ class "image-grid" ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ (Time.every Time.minute UpdateTime)
        , Sub.map AutocompleteMsg <| Autocomplete.subscriptions model.autocomplete
        ]
