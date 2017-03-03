module Model exposing (..)

import Geolocation


type alias Flags =
    { title : String
    , flickrApiKey : String
    }


type alias Model =
    { title : String
    , apiKey : String
    , loadingLocation : Bool
    , location : Maybe Geolocation.Location
    , locationError : Maybe String
    , loadingPlace : Bool
    , place : Maybe Place
    , placeLoadError : Maybe String
    }


type alias Place =
    { place_id : String
    , woeid : String
    , latitude : String
    , longitude : String
    , place_url : String
    , place_type : String
    , place_type_id : String
    , name : String
    , woe_name : String
    }


initialState : Flags -> ( Model, Cmd msg )
initialState flags =
    ( { title = flags.title
      , apiKey = flags.flickrApiKey
      , loadingLocation = False
      , location = Nothing
      , locationError = Nothing
      , loadingPlace = False
      , place = Nothing
      , placeLoadError = Nothing
      }
    , Cmd.none
    )
