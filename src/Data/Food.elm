module Data.Food exposing (Food, decoder, totalGrams)

import Dict exposing (Dict)
import Json.Decode as JD


decoder : JD.Decoder (Dict String (List Food))
decoder =
    JD.dict <|
        JD.list
            (JD.map4 Food
                (JD.field "Name" JD.string)
                (JD.field "Eiweiß" JD.float)
                (JD.field "FETT" JD.float)
                (JD.field "Kohlehy." JD.float)
            )


type alias Food =
    { name : String
    , protein : Float
    , fat : Float
    , carbs : Float
    }


totalGrams : Food -> Float
totalGrams { protein, fat, carbs } =
    protein + fat + carbs

