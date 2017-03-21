module Components.LocationAutocomplete exposing (..)

import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Regex exposing (regex)


-- Model & initial state


type alias Model =
    { query : String
    , maxResults : Int
    , showSuggestions : Bool
    , suggestions : List Location
    , selected : Maybe Location
    , loading : Bool
    , loadError : Maybe String
    , component : Autocomplete.State
    }


type alias Location =
    { name : String
    , lat : Int
    , lng : Int
    }


initialState : Model
initialState =
    { query = "Køben"
    , maxResults = 8
    , showSuggestions = True
    , suggestions =
        [ Location "Jegindø" 0 0
        , Location "Aarhus" 0 0
        , Location "København" 0 0
        , Location "København 2" 0 0
        , Location "København 3" 0 0
        , Location "Bornholnmsk" 0 0
        , Location "New York" 0 0
        , Location "Berlin" 0 0
        , Location "London" 0 0
        , Location "Silkeborg" 0 0
        ]
    , selected = Just (Location "København 2" 0 0)
    , loading = False
    , loadError = Nothing
    , component = Autocomplete.empty
    }



-- Update


type Msg
    = UpdateQuery String
    | UpdateComponent Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectItem String
    | PreviewItem String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery newInput ->
            ( { model
                | query = newInput
                , showSuggestions =
                    if newInput == "" then
                        False
                    else
                        not <| List.isEmpty <| filterResults model.query model.suggestions
              }
            , Cmd.none
            )

        UpdateComponent autoMsg ->
            let
                ( newComponentState, maybeMsg ) =
                    Autocomplete.update
                        updateComponentConf
                        autoMsg
                        model.maxResults
                        model.component
                        (filterResults model.query model.suggestions)

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
                results =
                    filterResults model.query model.suggestions

                stateAfterEscKey =
                    { model
                        | query =
                            if List.isEmpty results then
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
                Just person ->
                    update Reset model

                Nothing ->
                    let
                        results =
                            filterResults model.query model.suggestions

                        componentState =
                            if toTop then
                                Autocomplete.resetToLastItem updateComponentConf results model.maxResults model.component
                            else
                                Autocomplete.resetToFirstItem updateComponentConf results model.maxResults model.component

                        selectedItem =
                            if toTop then
                                List.head <| List.reverse <| List.take model.maxResults <| results
                            else
                                List.head <| List.take model.maxResults <| results
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
            , Cmd.none
            )

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



-- Filtering & sorting logic


getItem : List Location -> String -> Location
getItem suggestions id =
    List.filter (\item -> item.name == id) suggestions
        |> List.head
        |> Maybe.withDefault (Location "" 0 0)


isMatch : String -> Location -> Bool
isMatch query item =
    String.contains query (String.toLower item.name)


filterResults : String -> List Location -> List Location
filterResults query results =
    let
        lcQuery =
            String.toLower query
    in
        List.filter (isMatch lcQuery) results



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
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        results =
            filterResults model.query model.suggestions

        acView =
            Autocomplete.view (viewConfig model) model.maxResults model.component results
    in
        div [ class "autocomplete-menu" ]
            [ Html.map UpdateComponent acView ]



{-
   highlightMatches

    Wraps matches in `<strong>` elements and returns a list of Html
    If no matches are found then return a list with a single child
    containing the original string wrapped in a `<span>`
-}


highlightMatches : String -> String -> List (Html Never)
highlightMatches needle haystack =
    let
        needleLen =
            String.length needle

        haystackLen =
            String.length haystack

        lcNeedle =
            String.toLower needle

        lcHaystack =
            String.toLower haystack

        matches : List Regex.Match
        matches =
            Regex.find Regex.All (regex lcNeedle) lcHaystack

        matchByNumber : Int -> Maybe Regex.Match
        matchByNumber num =
            List.head <| List.filter (\match -> match.number == num) matches

        highlightMatch : Regex.Match -> Html Never
        highlightMatch match =
            let
                before =
                    if match.index == 0 then
                        ""
                    else
                        case matchByNumber (match.number - 1) of
                            Nothing ->
                                String.slice 0 match.index haystack

                            Just prevMatch ->
                                String.slice (prevMatch.index + 1) match.index haystack

                after =
                    case (List.head <| List.reverse matches) of
                        Nothing ->
                            ""

                        Just lastMatch ->
                            -- Check if this match is the last match we found
                            if lastMatch.number == match.number then
                                -- Check if last match is also last char in string. If not
                                -- then add the remaining characters
                                if (lastMatch.index + needleLen) == haystackLen then
                                    ""
                                else
                                    String.slice (lastMatch.index + needleLen) haystackLen haystack
                            else
                                ""
            in
                span []
                    [ text before
                    , strong [] [ text <| String.slice match.index (match.index + needleLen) haystack ]
                    , text after
                    ]
    in
        if List.isEmpty matches then
            [ span [] [ text haystack ] ]
        else
            List.map highlightMatch matches


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
