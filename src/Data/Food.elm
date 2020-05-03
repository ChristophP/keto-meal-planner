module Data.Food exposing
    ( Food
    , Nutrient(..)
    , caloriesPerGram
    , decoder
    , fromNameAndNutrients
    , getCalories
    , getNutrientCalories
    , getNutrientGrams
    , totalNutrientWeightPer100grams
    )

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


type Nutrient
    = Protein
    | Fat
    | Carbs


fromNameAndNutrients : String -> { protein : Float, fat : Float, carbs : Float } -> Food
fromNameAndNutrients name { protein, fat, carbs } =
    Food name protein fat carbs


parse :
    { a | name : String, protein : String, fat : String, carbs : String }
    -> Result (List String) Food
parse { name, protein, fat, carbs } =
    Debug.todo ""


parseName : String -> Result String String
parseName name =
    if String.length name >= 3 then
        Ok name

    else
        Err "Name is too short"


parseNutrient : String -> Result String Float
parseNutrient nutrient =
    case String.toFloat nutrient of
        Just val ->
            Ok (clamp 0 100 val)

        Nothing ->
            Err "Could not parse a number value"


type alias Food =
    { name : String
    , protein : Float
    , fat : Float
    , carbs : Float
    }


totalNutrientWeightPer100grams : Food -> Float
totalNutrientWeightPer100grams { protein, fat, carbs } =
    protein + fat + carbs


caloriesPerGram : { protein : Float, fat : Float, carbs : Float }
caloriesPerGram =
    { protein = 4, fat = 9, carbs = 4 }


getNutrientGrams : Nutrient -> Float -> Food -> Float
getNutrientGrams nutrient grams { protein, fat, carbs } =
    case nutrient of
        Protein ->
            protein / 100 * grams

        Fat ->
            fat / 100 * grams

        Carbs ->
            carbs / 100 * grams


getCalories : Float -> Food -> Float
getCalories grams food =
    List.sum
        [ getNutrientCalories Protein grams food
        , getNutrientCalories Fat grams food
        , getNutrientCalories Carbs grams food
        ]


getNutrientCalories : Nutrient -> Float -> Food -> Float
getNutrientCalories nutrient grams food =
    let
        factor =
            case nutrient of
                Protein ->
                    caloriesPerGram.protein

                Fat ->
                    caloriesPerGram.fat

                Carbs ->
                    caloriesPerGram.carbs
    in
    getNutrientGrams nutrient grams food * factor
