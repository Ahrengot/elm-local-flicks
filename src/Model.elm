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
