module View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
                viewImageGrid model.images
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


viewImageGrid : List Image -> Html Msg
viewImageGrid images =
    List.map viewImage images
        |> div [ class "image-grid" ]


viewImage : Image -> Html Msg
viewImage data =
    let
        -- https://farm{farm-id}.staticflickr.com/{server-id}/{id}_{secret}_[mstzb].jpg
        srcBase =
            "https://farm" ++ (toString data.farm) ++ ".staticflickr.com/" ++ data.server ++ "/" ++ data.id ++ "_" ++ data.secret
    in
        div [ class "grid-item" ]
            [ a [ href (srcBase ++ "_b.jpg") ]
                [ img [ Html.Attributes.src (srcBase ++ "_b.jpg") ] []
                ]
            ]
