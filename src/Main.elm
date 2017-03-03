port module Main exposing (..)

import Html
import View
import Model
import Msg exposing (..)
import Update


main : Program Model.Flags Model.Model Msg
main =
    Html.programWithFlags
        { init = Model.initialState
        , view = View.app
        , update = Update.update
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Sub.none
