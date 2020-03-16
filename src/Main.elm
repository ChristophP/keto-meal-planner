module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data.Food as Food
import Data.Meal as Meal
import Dict exposing (Dict)
import Html exposing (Html, button, div, li, ol, p, span, text, ul)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import List.Extra as LE
import String.Mark as Mark
import Task
import Util exposing (toFixed)
import View.Helpers as VH
import View.Icons as Icons


main : Program JD.Value Model Msg
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


type alias Foods =
    Result String (Dict String (List Food.Food))


type alias Model =
    { count : Int
    , showFoods : Bool
    , foods : Foods
    , searchTerm : String
    , selectedFoods : List Food.Food
    }


init : JD.Value -> ( Model, Cmd Msg )
init json =
    ( { count = 0
      , showFoods = False
      , foods =
            JD.decodeValue Food.decoder json
                |> Result.mapError (always "Could not decode food list")
      , searchTerm = ""
      , selectedFoods = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | FoodAdded
    | DialogCancelled
    | SearchChanged String
    | FoodClicked Food.Food
    | DeleteFoodClicked Food.Food
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        FoodAdded ->
            ( { model | showFoods = True }
            , Task.attempt (always NoOp) (Dom.focus foodSearchId)
            )

        DialogCancelled ->
            ( { model | showFoods = False }, Cmd.none )

        SearchChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        FoodClicked food ->
            ( { model
                | selectedFoods = food :: model.selectedFoods
                , showFoods = False
                , searchTerm = ""
              }
            , Cmd.none
            )

        DeleteFoodClicked food ->
            ( { model | selectedFoods = LE.remove food model.selectedFoods }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


foodSearchId : String
foodSearchId =
    "food-search"


viewNutrientPctg : Float -> Food.Food -> String
viewNutrientPctg num food =
    toFixed 1 (num / Food.totalGrams food * 100) ++ "%"


viewMealGrams : (Food.Food -> Float) -> List Food.Food -> String
viewMealGrams getter foods =
    let
        str =
            List.map getter foods
                |> List.sum
                |> toFixed 1
    in
    str ++ "g"


view : Model -> Browser.Document Msg
view model =
    let
        mealPctg =
            case LE.getAt model.count Meal.meals of
                Just meal ->
                    Meal.toPercentage meal

                Nothing ->
                    -- should never happen
                    0
    in
    Browser.Document "Keto Meal Planner"
        [ div [ class "flex flex-col w-full h-full" ] <|
            case model.foods of
                Ok foods ->
                    [ VH.slider
                        { items = List.map Meal.toString Meal.meals
                        , onBack = Decrement
                        , onNext = Increment
                        , index = model.count
                        }
                    , div [ class "flex-1 overflow-y-auto" ]
                        [ viewTotalNutrientsHeader model mealPctg
                        , ol [ class "mt-4 text-2xl text-center" ] <|
                            List.map
                                (\food ->
                                    li [ class "flex flex-col mt-2 bg-white shadow" ]
                                        [ div
                                            [ class "relative flex items-center justify-center"
                                            , class "text-sm font-semibold leading-relaxed"
                                            ]
                                            [ span [ class "text-indigo-700 " ] [ text food.name ]
                                            , button
                                                [ class "absolute right-0 inline-block w-4 mr-2 text-gray-400"
                                                , class "cursor-pointer hover:text-gray-600"
                                                , onClick <| DeleteFoodClicked food
                                                ]
                                                [ Icons.trash ]
                                            ]
                                        , div [ class "flex pb-1" ]
                                            [ div [ class "flex flex-col flex-1 px-2 text-sm" ]
                                                [ span [] [ text "Protein" ]
                                                , span [] [ text <| toFixed 2 food.protein, text "g" ]
                                                , span [] [ text (viewNutrientPctg food.protein food) ]
                                                ]
                                            , div [ class "flex flex-col flex-1 px-2 text-sm" ]
                                                [ span [ class "text-sm" ] [ text "Fat" ]
                                                , span [] [ text <| toFixed 2 food.fat, text "g" ]
                                                , span [] [ text (viewNutrientPctg food.fat food) ]
                                                ]
                                            , div [ class "flex flex-col flex-1 px-2 text-sm" ]
                                                [ span [] [ text "Carbs" ]
                                                , span [] [ text <| toFixed 2 food.carbs, text "g" ]
                                                , span [] [ text (viewNutrientPctg food.carbs food) ]
                                                ]
                                            ]
                                        ]
                                )
                                model.selectedFoods
                        ]
                    , button
                        [ class "bottom-0 w-16 h-16 mx-auto mb-2 bg-white rounded-full"
                        , class "text-indigo-600 shadow-lg hover:text-indigo-800"
                        , onClick FoodAdded
                        ]
                        [ Icons.addSolid ]
                    , VH.dialog
                        { show = model.showFoods
                        , title = "Pick Food"
                        , content =
                            [ div [ class "flex flex-col h-full" ]
                                [ VH.inputField
                                    [ id foodSearchId
                                    , placeholder "Search for Food"
                                    , onInput SearchChanged
                                    , value model.searchTerm
                                    ]
                                    []
                                , viewFoodsList model.searchTerm foods
                                ]
                            ]
                        , onClose = DialogCancelled
                        }
                    ]

                Err err ->
                    [ div [ class "w-full h-full text-xl" ] [ text err ] ]
        ]


caseInsensitiveSearch : String -> String -> Bool
caseInsensitiveSearch search word =
    String.contains (String.toLower search) (String.toLower word)


searchFoods : String -> Dict String (List Food.Food) -> Dict String (List Food.Food)
searchFoods searchTerm foods =
    case searchTerm of
        "" ->
            foods

        _ ->
            Dict.foldl
                (\key value acc ->
                    if caseInsensitiveSearch searchTerm key then
                        Dict.insert key value acc

                    else
                        let
                            filteredCategory =
                                List.filter
                                    (\food -> caseInsensitiveSearch searchTerm food.name)
                                    value
                        in
                        case filteredCategory of
                            [] ->
                                acc

                            _ ->
                                Dict.insert key filteredCategory acc
                )
                Dict.empty
                foods


viewFoodsList : String -> Dict String (List Food.Food) -> Html Msg
viewFoodsList searchTerm foods =
    let
        pairs =
            Dict.toList <| searchFoods searchTerm foods
    in
    div [ class "flex-1 overflow-y-auto v-gap" ] <|
        List.map
            (\( category, items ) ->
                div [ class "px-4" ]
                    [ p
                        [ class "pt-1 pb-2 border-t-2 border-indigo-700"
                        , class "antialiased font-semibold text-indigo-700"
                        ]
                      <|
                        Mark.mark searchTerm category
                    , ul [ class "mt-2" ] <|
                        List.map
                            (\item ->
                                li
                                    [ class "py-2 pl-2 text-lg cursor-pointer font-italics"
                                    , class "hover:bg-gray-100 active:bg-gray-100"
                                    , onClick <| FoodClicked item
                                    ]
                                    (Mark.mark searchTerm item.name)
                            )
                            items
                    ]
            )
            pairs


viewTotalNutrientsHeader : Model -> Float -> Html Msg
viewTotalNutrientsHeader model mealPctg =
    div [ class "mt-2 text-2xl text-center bg-white" ]
        [ span [ class "text-sm tracking-widest uppercase" ] [ text "Target calories" ]
        , div [ class "flex" ]
            [ div [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                [ span [] [ text "Protein" ]
                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.protein * mealPctg / caloriesPerGram.protein), text "g" ]
                , span [] [ text (String.fromFloat (targetNutritionRatio.protein * 100) ++ "%") ]
                , span [ class "font-medium text-indigo-700" ] [ text (viewMealGrams .protein model.selectedFoods) ]
                ]
            , div [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                [ span [ class "text-sm" ] [ text "Fat" ]
                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.fat * mealPctg / caloriesPerGram.fat), text "g" ]
                , span [] [ text (String.fromFloat (targetNutritionRatio.fat * 100) ++ "%") ]
                , span [ class "font-medium text-indigo-700" ] [ text (viewMealGrams .fat model.selectedFoods) ]
                ]
            , div [ class "flex flex-col flex-1 p-2 text-sm" ]
                [ span [] [ text "Carbs" ]
                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.carbs * mealPctg / caloriesPerGram.carbs), text "g" ]
                , span [] [ text (String.fromFloat (targetNutritionRatio.carbs * 100) ++ "%") ]
                , span [ class "font-medium text-indigo-700" ] [ text (viewMealGrams .carbs model.selectedFoods) ]
                ]
            ]
        ]
