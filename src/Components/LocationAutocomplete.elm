module Components.LocationAutocomplete exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { query : String
    , loading : Bool
    , loadError : Maybe String
    , suggestions : List String
    }


type Msg
    = ChangeInput String


initialState : Model
initialState =
    Model "" False Nothing []


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            { model | query = newInput }


view : Model -> Html Msg
view model =
    div [ class "location-autocomplete" ]
        [ input
            [ type_ "text"
            , onInput ChangeInput
            , value model.query
            , placeholder "Search for any location"
            ]
            []
        ]
