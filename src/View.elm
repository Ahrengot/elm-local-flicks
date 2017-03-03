module View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


app : Model -> Html Msg
app model =
    let
        locBtnText =
            if model.loadingLocation then
                "Loading..."
            else
                "Get my location"

        msg =
            case model.location of
                Just loc ->
                    "You are here: (" ++ (toString loc.latitude) ++ ", " ++ (toString loc.longitude) ++ ")"

                Nothing ->
                    ""
    in
        div [ class "app-container" ]
            [ div []
                [ header [ class "header" ]
                    [ h1 [] [ text model.title ]
                    , button
                        [ class "btn btn-default"
                        , type_ "button"
                        , onClick RequestLocation
                        , disabled model.loadingLocation
                        ]
                        [ text locBtnText
                        ]
                    ]
                , p
                    [ class "app-desc"
                    , style
                        [ ( "font-size", "80%" )
                        , ( "font-style", "italic" )
                        , ( "text-align", "center" )
                        , ( "margin-top", "3rem" )
                        ]
                    ]
                    [ text msg
                    ]
                ]
            ]
