module Data.Meal exposing (..)


type Meal
    = Breakfast Float
    | MorningSnack Float
    | Lunch Float
    | Snack Float
    | Dinner Float


meals =
    [ Breakfast 0.2
    , MorningSnack 0.15
    , Lunch 0.25
    , Snack 0.15
    , Dinner 0.25
    ]


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
