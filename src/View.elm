module View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Date
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Geolocation
import Date.Distance as TimeAgo
import Util exposing (distanceInKm)


app : Model -> Html Msg
app model =
    let
        loadBtn =
            if (List.length model.images) > 0 then
                p [ class "results-description" ] [ text ("Found " ++ (toString (List.length model.images)) ++ " images.") ]
            else
                viewGetLocationBtn (model.loadingPlace || model.loadingLocation || model.loadingImages)

        locationErr =
            case model.locationLoadError of
                Nothing ->
                    text ""

                Just err ->
                    viewLoadError err

        placeErr =
            case model.placeLoadError of
                Nothing ->
                    text ""

                Just err ->
                    viewLoadError err

        titleTxt =
            case model.place of
                Nothing ->
                    model.title

                Just loc ->
                    loc.woe_name

        imgGrid =
            if (List.length model.images) > 0 then
                viewImageGrid model
            else
                text ""
    in
        div [ class "app-container" ]
            [ div []
                [ header [ class "header" ]
                    [ h1 [] [ text titleTxt ]
                    , loadBtn
                    ]
                , locationErr
                , placeErr
                , imgGrid
                ]
            ]


viewGetLocationBtn : Bool -> Html Msg
viewGetLocationBtn loading =
    let
        btnText =
            if loading then
                "Loading..."
            else
                "Show me images!"
    in
        button
            [ class "btn btn-primary"
            , type_ "button"
            , onClick RequestLocation
            , disabled loading
            ]
            [ text btnText
            ]


viewLoadError : String -> Html Msg
viewLoadError msg =
    div [ class "alert alert-danger" ]
        [ p [] [ text msg ] ]


viewImageGrid : Model -> Html Msg
viewImageGrid model =
    List.map (viewImage model.location model.now) model.images
        |> div [ class "image-grid" ]


distance : Geolocation.Location -> { lat : Float, lon : Float } -> String
distance posA posB =
    (toString posB.lat) ++ ", " ++ (toString posB.lon)


viewImage : Maybe Geolocation.Location -> Float -> Image -> Html Msg
viewImage userLocation now imgData =
    let
        -- https://farm{farm-id}.staticflickr.com/{server-id}/{id}_{secret}_[mstzb].jpg
        srcBase =
            "https://farm" ++ (toString imgData.farm) ++ ".staticflickr.com/" ++ imgData.server ++ "/" ++ imgData.id ++ "_" ++ imgData.secret

        imgLocation =
            { lat = Result.withDefault 0 (String.toFloat imgData.latitude)
            , lon = Result.withDefault 0 (String.toFloat imgData.longitude)
            }

        distanceStr =
            if imgLocation.lat /= 0 && imgLocation.lon /= 0 then
                case userLocation of
                    Nothing ->
                        "Unavailable"

                    Just loc ->
                        distanceInKm loc.longitude loc.latitude imgLocation.lon imgLocation.lat
            else
                "Unavailable"
    in
        div [ class "grid-item" ]
            [ div [ class "img-card" ]
                [ a [ href (srcBase ++ "_b.jpg") ]
                    [ img [ Html.Attributes.src <| srcBase ++ "_z.jpg" ] []
                    ]
                , div [ class "img-meta" ]
                    [ div [ class "img-date" ]
                        [ span [ class "icon icon-date" ] []
                        , text <| (TimeAgo.inWords (Date.fromTime now) imgData.date) ++ " ago"
                        ]
                    , div [ class "img-distance" ]
                        [ span [ class "icon icon-location" ] []
                        , text distanceStr
                        ]
                    ]
                ]
            ]
