module Data.Session exposing (Session, init)

import Browser.Navigation as Nav
import Data.Food as Food
import Dict exposing (Dict)


type alias Foods =
    Result String (Dict String (List Food.Food))


type alias Session =
    { navKey : Nav.Key
    , foods : Foods
    }


init : Nav.Key -> Foods -> Session
init key foods =
    { navKey = key, foods = foods }
