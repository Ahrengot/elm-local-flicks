module Router exposing (..)

import Navigation
import Http exposing (encodeUri)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf, parseHash)


-- Model & initial state


type alias Model =
    { route : Route
    , history : List Navigation.Location
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


type Route
    = Home
    | LocationSearch String ( Float, Float )
    | NotFound


initialState : Navigation.Location -> Model
initialState initialLocation =
    Model (locationToRoute initialLocation) [ initialLocation ]



-- Update


type Msg
    = Navigate String
    | UrlChange Navigation.Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Navigate hash ->
            Debug.log "Navigate not yet implemented" model

        UrlChange location ->
            { model
                | history = location :: model.history
                , route = locationToRoute location
            }



-- Utility functions


locationToHash : Location -> String
locationToHash loc =
    "#/location/" ++ (encodeUri loc.name) ++ "/@/" ++ (toString loc.lat) ++ "," ++ (toString loc.lng)


locationToRoute : Navigation.Location -> Route
locationToRoute location =
    -- If hash parsing fails fall back to NotFound route.
    -- By doing it this way we can prevent all components doing pattern
    -- matching on this component from having to deal with maybe's
    case (parseHash hashToRoute location) of
        Just route ->
            route

        Nothing ->
            NotFound


hashToRoute : Parser (Route -> a) a
hashToRoute =
    oneOf
        [ map Home UrlParser.top
        , map LocationSearch (s "location" </> string </> s "@" </> latLngParser)
        ]


latLngParser : Parser (( Float, Float ) -> b) b
latLngParser =
    UrlParser.custom "LAT_LNG" <|
        \segment ->
            let
                values =
                    String.split "," segment
                        |> List.filterMap
                            (\strVal ->
                                case String.toFloat strVal of
                                    Ok val ->
                                        Just val

                                    Err error ->
                                        Nothing
                            )

                lat =
                    Maybe.withDefault 0 <| List.head values

                lng =
                    Maybe.withDefault 0 <| List.head <| List.reverse values
            in
                if 2 == List.length values then
                    Ok ( lat, lng )
                else
                    Err "Segment doesn't match format: <float>,<float>"
