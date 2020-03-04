module Main exposing (main)

import Browser
import Data.Meal as Meal
import Html exposing (Html, button, div, li, ol, span, text, ul)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import List.Extra as LE
import Util exposing (toFixed)
import View.Icons as Icons


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


totalAllowedCalories =
    800


targetNutritionRatio =
    { protein = 0.08, fat = 0.84, carbs = 0.08 }


caloriesPerGram =
    { protein = 4, fat = 9, carbs = 4 }


type alias Model =
    { count : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        mealPctg =
            case LE.getAt model.count Meal.meals of
                Just meal ->
                    Meal.toPercentage meal

                Nothing ->
                    0

        -- should never happen
    in
    Browser.Document "Keto Meal Planner"
        [ div [ class "mt-auto text-2xl text-center bg-white" ]
            [ text "Target calories"
            , ol [ class "flex" ]
                [ li [ class "flex flex-1 flex-col p-2 border-r border-black text-sm" ]
                    [ span [] [ text "Protein" ]
                    , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.protein * mealPctg / caloriesPerGram.protein), text "g" ]
                    , span [] [ text (String.fromFloat (targetNutritionRatio.protein * 100) ++ "%") ]
                    ]
                , li [ class "flex flex-1 flex-col p-2 border-r border-black text-sm" ]
                    [ span [ class "text-sm" ] [ text "Fat" ]
                    , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.fat * mealPctg / caloriesPerGram.fat), text "g" ]
                    , span [] [ text (String.fromFloat (targetNutritionRatio.fat * 100) ++ "%") ]
                    ]
                , li [ class "flex flex-1 flex-col p-2 text-sm" ]
                    [ span [] [ text "Carbs" ]
                    , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.carbs * mealPctg / caloriesPerGram.carbs), text "g" ]
                    , span [] [ text (String.fromFloat (targetNutritionRatio.carbs * 100) ++ "%") ]
                    ]
                ]
            ]
        , div [ class "py-1 flex justify-between items-center border-t border-black text-center text-3xl bg-white shadow-md" ]
            [ button
                [ class "w-24", onClick Decrement, disabled (model.count <= 0) ]
                [ Icons.chevronLeft ]
            , div [ class "flex-1 overflow-hidden" ]
                [ ul
                    [ class "flex flex-full items-center transition-tranform duration-500"
                    , style "transform" ("translateX(-" ++ String.fromInt (model.count * 100) ++ "%)")
                    ]
                  <|
                    List.map viewMeal Meal.meals
                ]
            , button [ class "w-24", onClick Increment, disabled (model.count >= List.length Meal.meals - 1) ] [ Icons.chevronRight ]
            ]
        ]


viewMeal meal =
    li [ class "flex-full px-2 text-center" ] [ text <| Meal.toString meal ]
