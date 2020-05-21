module Data.Meal exposing
    ( Meal
    , getCalories
    , getKDFactor
    , meals
    , toPercentage
    , toString
    )

import Data.Food as Food exposing (Food)


type Meal
    = Breakfast Float
    | MorningSnack Float
    | Lunch Float
    | Snack Float
    | Dinner Float


meals : List Meal
meals =
    [ Breakfast 0.2
    , MorningSnack 0.15
    , Lunch 0.25
    , Snack 0.15
    , Dinner 0.25
    ]


toString : Meal -> String
toString meal =
    case meal of
        Breakfast _ ->
            "Breakfast"

        MorningSnack _ ->
            "Morning Snack"

        Lunch _ ->
            "Lunch"

        Snack _ ->
            "Snack"

        Dinner _ ->
            "Dinner"


toPercentage : Meal -> Float
toPercentage meal =
    case meal of
        Breakfast val ->
            val

        MorningSnack val ->
            val

        Lunch val ->
            val

        Snack val ->
            val

        Dinner val ->
            val


type alias FoodPortions =
    List ( Float, Food )


getCalories : FoodPortions -> Float
getCalories =
    List.map (\( grams, food ) -> Food.getCalories grams food)
        >> List.sum


getGrams : FoodPortions -> Float
getGrams =
    List.map Tuple.first >> List.sum


{-| The KD Factor indicated the relationship of grams of fat to grams of protein and carbs.
It is computed as a weighted average of the food's KD factor.
-}
getKDFactor : FoodPortions -> Float
getKDFactor foodPortions =
    List.foldl (\( grams, food ) kdFactor -> kdFactor + grams * Food.getKDFactor food) 0 foodPortions
        / getGrams foodPortions
