module Msg exposing (..)

import Geolocation exposing (Error, Location)
import Http
import Model exposing (Place, Image)


type Msg
    = RequestLocation
    | ReceivedLocation Location
    | LocationFailed String
    | PlaceResponse (Result Http.Error (List Place))
    | ImagesResponse (Result Http.Error (List Image))
