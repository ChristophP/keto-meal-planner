module Data.Session exposing (Session, addFoods, hasFoods, init)

import Browser.Navigation as Nav
import Data.Food as Food
import Dict exposing (Dict)


type alias Foods =
    Dict String (List Food.Food)


type alias Session =
    { navKey : Nav.Key
    , foods : Foods
    , navOpen : Bool
    , settingsOpen : Bool
    }


init : Nav.Key -> Session
init key =
    { navKey = key
    , foods = Dict.empty
    , navOpen = False
    , settingsOpen = False
    }


addFoods : Foods -> Session -> Session
addFoods foods session =
    { session | foods = foods }


hasFoods : Session -> Bool
hasFoods =
    not << Dict.isEmpty << .foods
