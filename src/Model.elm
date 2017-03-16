module Model exposing (..)

import Date
import Geolocation


type alias Flags =
    { title : String
    , flickrApiKey : String
    }


type alias Model =
    { title : String
    , apiKey : String
    , now : Float
    , loadingLocation : Bool
    , location : Maybe Geolocation.Location
    , locationLoadError : Maybe String
    , loadingPlace : Bool
    , place : Maybe Place
    , placeLoadError : Maybe String
    , images : List Image
    , loadingImages : Bool
    , imagesLoadError : Maybe String
    }


type alias Image =
    { id : String
    , owner : String
    , secret : String
    , server : String
    , farm : Int
    , title : String
    , date : Date.Date
    , longitude : String
    , latitude : String
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
      , now = 0
      , loadingLocation = False
      , location = Nothing
      , locationLoadError = Nothing
      , loadingPlace = False
      , place = Nothing
      , placeLoadError = Nothing
      , images = []
      , loadingImages = False
      , imagesLoadError = Nothing
      }
    , Cmd.none
    )
