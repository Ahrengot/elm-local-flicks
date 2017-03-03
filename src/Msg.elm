module Msg exposing (..)

import Geolocation exposing (Error, Location)
import Http
import Model exposing (Place)


type Msg
    = RequestLocation
    | ReceivedLocation Location
    | LocationFailed String
    | RequestPlace
    | PlaceResponse (Result Http.Error Place)
