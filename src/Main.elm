module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data.Food as Food
import Data.Meal as Meal
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, label, li, ol, option, p, select, span, text, ul)
import Html.Attributes exposing (class, disabled, for, id, placeholder, style)
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
    | AddFood
    | CancelDialog
    | ChangeSearch String
    | FoodClicked Food.Food
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        AddFood ->
            ( { model | showFoods = True }
            , Task.attempt (always NoOp) (Dom.focus "food-search")
            )

        CancelDialog ->
            ( { model | showFoods = False }, Cmd.none )

        ChangeSearch searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        FoodClicked food ->
            ( { model
                | selectedFoods = food :: model.selectedFoods
                , showFoods = False
              }
            , Cmd.none
            )

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
                    , div [ class "text-2xl text-center bg-white" ]
                        [ text "Target calories"
                        , ol [ class "flex" ]
                            [ li [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                                [ span [] [ text "Protein" ]
                                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.protein * mealPctg / caloriesPerGram.protein), text "g" ]
                                , span [] [ text (String.fromFloat (targetNutritionRatio.protein * 100) ++ "%") ]
                                ]
                            , li [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                                [ span [ class "text-sm" ] [ text "Fat" ]
                                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.fat * mealPctg / caloriesPerGram.fat), text "g" ]
                                , span [] [ text (String.fromFloat (targetNutritionRatio.fat * 100) ++ "%") ]
                                ]
                            , li [ class "flex flex-col flex-1 p-2 text-sm" ]
                                [ span [] [ text "Carbs" ]
                                , span [] [ text <| toFixed 2 (totalAllowedCalories * targetNutritionRatio.carbs * mealPctg / caloriesPerGram.carbs), text "g" ]
                                , span [] [ text (String.fromFloat (targetNutritionRatio.carbs * 100) ++ "%") ]
                                ]
                            ]
                        ]
                    , button
                        [ class "w-24 h-24 mx-auto mt-auto mb-2 rounded-full"
                        , class "text-indigo-600 hover:text-indigo-800"
                        , onClick AddFood
                        ]
                        [ Icons.addSolid ]
                    , VH.dialog
                        { show = model.showFoods
                        , title = "Pick Food"
                        , content =
                            [ div [ class "flex flex-col h-full" ]
                                [ VH.inputField
                                    [ id "food-search"
                                    , placeholder "Search for Food"
                                    , onInput ChangeSearch
                                    ]
                                    []
                                , viewFoodsList model.searchTerm foods
                                ]
                            ]
                        , onClose = CancelDialog
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
