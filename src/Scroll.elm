port module Scroll exposing (..)

-- Model & initial state


type alias Model =
    { reachedEndOfPage : Bool
    , bottomOfWindow : Int
    , isListening : Bool
    }


initialState : Model
initialState =
    Model False 0 False


type Msg
    = StopListening
    | StartListening
    | Scroll ( Int, Bool )



-- Update


port toggleScroll : Bool -> Cmd msg


port onScroll : (( Int, Bool ) -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StopListening ->
            ( { model | isListening = False }, toggleScroll False )

        StartListening ->
            ( { model | isListening = True }, toggleScroll True )

        Scroll ( bottomOfWindow, didReachEnd ) ->
            ( { model
                | reachedEndOfPage = didReachEnd
                , bottomOfWindow = bottomOfWindow
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isListening then
        onScroll Scroll
    else
        Sub.none
