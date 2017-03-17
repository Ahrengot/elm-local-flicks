port module Main exposing (..)

import Html
import View
import Model
import Msg exposing (..)
import Update
import Time
import Task


onGetInitialTime : Result error Float -> Msg
onGetInitialTime result =
    case result of
        Ok time ->
            ReceivedTime time

        Err err ->
            ReceivedTime 0


initialState : Model.Flags -> ( Model.Model, Cmd Msg )
initialState flags =
    ( { title = flags.title
      , apiKey = flags.flickrApiKey
      , now = 0
      , loadingLocation = False
      , location = Nothing
      , locationLoadError = Nothing
      , images = []
      , loadingImages = False
      , imagesLoadError = Nothing
      }
    , Task.attempt onGetInitialTime Time.now
    )


main : Program Model.Flags Model.Model Msg
main =
    Html.programWithFlags
        { init = initialState
        , view = View.app
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Time.every (10 * Time.second) ReceivedTime
