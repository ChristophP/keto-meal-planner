module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data.Meal as Meal
import Html exposing (Html, button, div, label, li, ol, option, select, span, text, ul)
import Html.Attributes exposing (class, disabled, for, id, style)
import Html.Events exposing (onClick)
import List.Extra as LE
import Task
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
    { count : Int, showFoods : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0, showFoods = False }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | AddFood
    | CancelDialog
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        AddFood ->
            ( { model | showFoods = True }, Cmd.none )

        CancelDialog ->
            ( { model | showFoods = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


foodSelectId =
    "food-select"


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
        [ div [ class "flex justify-between items-center border-b border-black text-center text-2xl bg-white shadow-md" ]
            [ button
                [ class "w-20", onClick Decrement, disabled (model.count <= 0) ]
                [ Icons.chevronLeft ]
            , div [ class "flex-1 overflow-hidden" ]
                [ ul
                    [ class "flex flex-full items-center transition-tranform duration-500"
                    , style "transform" ("translateX(-" ++ String.fromInt (model.count * 100) ++ "%)")
                    ]
                  <|
                    List.map viewMeal Meal.meals
                ]
            , button [ class "w-20", onClick Increment, disabled (model.count >= List.length Meal.meals - 1) ] [ Icons.chevronRight ]
            ]
        , div [ class "text-2xl text-center bg-white" ]
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
        , button
            [ class "mt-auto mx-auto w-24 h-24 rounded-full text-blue-400"
            , onClick AddFood
            ]
            [ Icons.addSolid ]
        , viewModal model.showFoods
        ]


viewModal open =
    if open then
        div [ class "fixed inset-0 w-screen h-screen flex items-center justify-center p-4" ]
            [ div [ class "absolute inset-0 bg-gray-400 opacity-50" ] []
            , div [ class "abolute w-full h-full bg-white rounded-lg z-10" ]
                [ div [ class "border-b border-gray-400 py-2 text-center text-xl relative" ]
                    [ text "Pick Food"
                    , button [ class "absolute w-8 h-8 right-0 top-0 mt-2 mr-2", onClick CancelDialog ] [ Icons.close ]
                    ]
                ]
            ]

    else
        text ""


viewMeal meal =
    li [ class "flex-full px-2 text-center" ] [ text <| Meal.toString meal ]
