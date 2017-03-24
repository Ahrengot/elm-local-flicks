module Components.LocationAutocomplete exposing (..)

import Autocomplete
import Debounce
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import Json.Decode as JD
import Util exposing (highlightMatches, parseHttpError)


-- Model & initial state


type alias Model =
    { query : String
    , maxResults : Int
    , showSuggestions : Bool
    , suggestions : List Location
    , selected : Maybe Location
    , searchDebouncer : Debounce.Model String
    , loading : Bool
    , loadError : Maybe String
    , component : Autocomplete.State
    }


type alias Location =
    { name : String
    , lat : Float
    , lng : Float
    }


initialState : Model
initialState =
    { query = ""
    , maxResults = 8
    , showSuggestions = False
    , suggestions = []
    , selected = Nothing
    , searchDebouncer = Debounce.init 600 ""
    , loading = False
    , loadError = Nothing
    , component = Autocomplete.empty
    }



-- Update


type Msg
    = UpdateQuery String
    | DebouncerMsg (Debounce.Msg String)
    | GmapsSuggestionsResponse (Result Http.Error (List Location))
    | UpdateComponent Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectItem String
    | AfterSelectItem Location
    | PreviewItem String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newInput ->
            let
                ( newDebouncer, cmd, settledMaybe ) =
                    Debounce.update (Debounce.Change newInput) model.searchDebouncer

                newModel =
                    { model
                        | query = newInput
                        , loading =
                            if newInput == "" then
                                False
                            else
                                model.loading
                        , showSuggestions =
                            if newInput == "" then
                                False
                            else
                                not <| List.isEmpty model.suggestions
                        , searchDebouncer = newDebouncer
                    }
            in
                ( newModel, Cmd.map DebouncerMsg cmd )

        DebouncerMsg dMsg ->
            let
                ( newDebouncer, debounceCmd, settledMaybe ) =
                    Debounce.update dMsg model.searchDebouncer

                cmd =
                    case settledMaybe of
                        Nothing ->
                            Cmd.none

                        Just _ ->
                            if model.query == "" then
                                Cmd.none
                            else
                                fetchSuggestions model
            in
                ( { model
                    | searchDebouncer = newDebouncer
                    , suggestions =
                        if cmd == Cmd.none then
                            model.suggestions
                        else
                            []
                    , loadError = Nothing
                    , loading =
                        if cmd == Cmd.none then
                            model.loading
                        else
                            True
                  }
                , cmd
                )

        GmapsSuggestionsResponse response ->
            case response of
                Ok suggestions ->
                    ( { model | loading = False, suggestions = suggestions, showSuggestions = True }, Cmd.none )

                Err error ->
                    ( { model | loading = False, loadError = Just <| parseHttpError error }, Cmd.none )

        UpdateComponent autoMsg ->
            let
                ( newComponentState, maybeMsg ) =
                    Autocomplete.update
                        updateComponentConf
                        autoMsg
                        model.maxResults
                        model.component
                        model.suggestions

                newModel =
                    { model | component = newComponentState }
            in
                case maybeMsg of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just updateMsg ->
                        update updateMsg newModel

        HandleEscape ->
            let
                stateAfterEscKey =
                    { model
                        | query =
                            if List.isEmpty model.suggestions then
                                ""
                            else
                                model.query
                        , selected = Nothing
                        , showSuggestions = False
                        , component = Autocomplete.empty
                    }

                newModel =
                    case model.selected of
                        Just item ->
                            if model.query == item.name then
                                -- If user selects item matching current query then
                                -- force reset query no matter if we have results or not
                                { stateAfterEscKey | query = "" }
                            else
                                stateAfterEscKey

                        Nothing ->
                            stateAfterEscKey
            in
                ( newModel, Cmd.none )

        Wrap toTop ->
            case model.selected of
                Just item ->
                    update Reset model

                Nothing ->
                    let
                        componentState =
                            if toTop then
                                Autocomplete.resetToLastItem updateComponentConf model.suggestions model.maxResults model.component
                            else
                                Autocomplete.resetToFirstItem updateComponentConf model.suggestions model.maxResults model.component

                        selectedItem =
                            if toTop then
                                List.head <| List.reverse <| List.take model.maxResults <| model.suggestions
                            else
                                List.head <| List.take model.maxResults <| model.suggestions
                    in
                        ( { model
                            | component = componentState
                            , selected = selectedItem
                          }
                        , Cmd.none
                        )

        Reset ->
            ( { model
                | component = Autocomplete.reset updateComponentConf model.component
                , selected = Nothing
              }
            , Cmd.none
            )

        SelectItem id ->
            ( { model
                | query = id
                , component = Autocomplete.empty
                , showSuggestions = False
              }
            , case model.selected of
                Nothing ->
                    Cmd.none

                Just location ->
                    Task.perform AfterSelectItem <| Task.succeed location
            )

        AfterSelectItem location ->
            ( model, Cmd.none )

        PreviewItem id ->
            ( { model
                | selected = Just <| getItem model.suggestions id
              }
            , Cmd.none
            )

        NoOp ->
            model ! []


updateComponentConf : Autocomplete.UpdateConfig Msg Location
updateComponentConf =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewItem maybeId
                else if code == 13 then
                    Maybe.map SelectItem maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewItem id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectItem id
        , separateSelections = False
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateComponent Autocomplete.subscription



-- Http & Http response decoding


fetchSuggestions : Model -> Cmd Msg
fetchSuggestions model =
    let
        url =
            "https://maps.googleapis.com/maps/api/geocode/json?address=" ++ (Http.encodeUri model.query) ++ "&client_id="
    in
        Http.get url suggestionListDecoder
            |> Http.send GmapsSuggestionsResponse


suggestionListDecoder : JD.Decoder (List Location)
suggestionListDecoder =
    JD.at [ "results" ] (JD.list suggestionDecoder)


suggestionDecoder : JD.Decoder Location
suggestionDecoder =
    JD.map3
        Location
        (JD.field "formatted_address" JD.string)
        (JD.at [ "geometry", "location", "lat" ] JD.float)
        (JD.at [ "geometry", "location", "lng" ] JD.float)



-- Filtering & sorting logic


getItem : List Location -> String -> Location
getItem suggestions id =
    List.filter (\item -> item.name == id) suggestions
        |> List.head
        |> Maybe.withDefault (Location "" 0 0)



-- View


handleInputKeyDown : JD.Decoder Msg
handleInputKeyDown =
    -- Handle special inputs like Esc key and arrow keys
    let
        resFromCode : Int -> Result String Msg
        resFromCode code =
            if code == 38 || code == 40 then
                Ok NoOp
            else if code == 27 then
                Ok HandleEscape
            else
                Err "not handling that key"
    in
        JD.map resFromCode keyCode
            |> JD.andThen
                (\result ->
                    case result of
                        Ok val ->
                            JD.succeed val

                        Err reason ->
                            JD.fail reason
                )


view : Model -> Html Msg
view model =
    div [ class "autocomplete autocomplete-location" ]
        [ input
            [ type_ "search"
            , class "autocomplete-input"
            , autocomplete False
            , onInput UpdateQuery
            , onWithOptions "keydown" { preventDefault = True, stopPropagation = False } handleInputKeyDown
            , value model.query
            , placeholder "Ie. Manhattan, New York"
            ]
            []
        , if model.showSuggestions then
            viewMenu model
          else
            text ""
        , if model.loading then
            viewLoadIndicator
          else
            text ""
        , case model.loadError of
            Nothing ->
                text ""

            Just err ->
                viewLoadError err
        ]


viewLoadIndicator : Html Msg
viewLoadIndicator =
    div [ class "autocomplete-load-indicator" ] [ text "Loading..." ]


viewLoadError : String -> Html Msg
viewLoadError err =
    div [ class "autocomplete-load-error" ] [ text err ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        acView =
            Autocomplete.view (viewConfig model) model.maxResults model.component model.suggestions
    in
        div [ class "autocomplete-menu" ]
            [ Html.map UpdateComponent acView ]


viewConfig : Model -> Autocomplete.ViewConfig Location
viewConfig model =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                ]
            , children = (highlightMatches model.query item.name)
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }
