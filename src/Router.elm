module Router exposing (..)

import Navigation
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf, parseHash)


-- Model & initial state


type alias Model =
    { route : Maybe Route
    , history : List Navigation.Location
    }


type Route
    = Home
    | LocationSearch String


initialState : Navigation.Location -> Model
initialState initialLocation =
    Model (parseHash hashToRoute initialLocation) [ initialLocation ]



-- Update


type Msg
    = NavigateTo String
    | UrlChange Navigation.Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        NavigateTo hash ->
            Debug.log "NavigateTo not yet implemented" model

        UrlChange location ->
            { model
                | history = location :: model.history
                , route = parseHash hashToRoute location
            }



-- Utility functions


hashToRoute : Parser (Route -> a) a
hashToRoute =
    oneOf
        [ map Home UrlParser.top
        , map LocationSearch (s "location" </> string </> s "@")
        ]



-- #/location/lala/@
-- lala@10.53,6,27
