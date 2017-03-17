module Msg exposing (..)

import Geolocation exposing (Error, Location)
import Http
import Model exposing (Place, Image)


type Msg
    = RequestLocation
    | ReceivedLocation Location
    | LocationFailed String
    | ReceivedTime Float
    | ImagesResponse (Result Http.Error (List Image))
