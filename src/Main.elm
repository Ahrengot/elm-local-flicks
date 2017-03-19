port module Main exposing (..)

import Html
import Time
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Components.LocationAutocomplete as Autocomplete
import UserLocation
import FlickrImages


-- Initial model and state


type alias Flags =
    { title : String
    , flickrApiKey : String
    }


type alias Model =
    { title : String
    , apiKey : String
    , now : Float
    , autocomplete : Autocomplete.Model
    , location : UserLocation.Model
    , flickrImages : FlickrImages.Model
    }


initialState : Flags -> ( Model, Cmd Msg )
initialState flags =
    ( { title = flags.title
      , apiKey = flags.flickrApiKey
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
            ( { model
                | autocomplete = Autocomplete.update acMsg model.autocomplete
              }
            , Cmd.none
            )

        LocationMsg locMsg ->
            let
                ( locationState, locCmd ) =
                    UserLocation.update locMsg model.location

                thisCmd =
                    case locMsg of
                        UserLocation.ReceivedLocation loc ->
                            Cmd.map FlickrMsg <| FlickrImages.fetchImages loc model.apiKey

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
        , view = app
        , update = update
        , subscriptions = subscriptions
        }


app : Model -> Html Msg
app model =
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
                    , Html.map AutocompleteMsg <| Autocomplete.view model.autocomplete
                    , div [ class "btn-group" ]
                        [ Html.map LocationMsg <| UserLocation.viewGetLocationBtn model.location
                        ]
                    ]
                , div [ class "errors" ] errors
                , Html.map FlickrMsg <| viewImageGrid model
                ]
            ]


viewError : String -> Html Msg
viewError msg =
    div [ class "alert alert-danger" ]
        [ p [] [ text msg ] ]


viewImageGrid : Model -> Html FlickrImages.Msg
viewImageGrid model =
    model.flickrImages.results
        |> List.map (FlickrImages.viewImage model.location.location model.now)
        |> div [ class "image-grid" ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (10 * Time.second) UpdateTime
