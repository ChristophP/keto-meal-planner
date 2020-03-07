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
    }


init : JD.Value -> ( Model, Cmd Msg )
init json =
    ( { count = 0
      , showFoods = False
      , foods =
            JD.decodeValue Food.decoder json
                |> Result.mapError (always "Could not decode food list")
      , searchTerm = ""
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
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        AddFood ->
            ( { model | showFoods = True }, Task.attempt (always NoOp) (Dom.focus "food-search") )

        CancelDialog ->
            ( { model | showFoods = False }, Cmd.none )

        ChangeSearch searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

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
        [ div [ class "w-full h-full flex flex-col" ] <|
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
                    , VH.dialog
                        { show = model.showFoods
                        , title = "Pick Food"
                        , content =
                            [ div [ class "h-full flex flex-col" ]
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
    div [ class "flex-1 overflow-y-auto p-2 v-gap-sm" ] <|
        List.map
            (\( category, items ) ->
                div []
                    [ p [ class "font-bold" ] <| Mark.mark searchTerm category
                    , ul [] <|
                        List.map
                            (\item ->
                                li
                                    [ class "font-italics pl-2 py-2" ]
                                    (Mark.mark searchTerm item.name)
                            )
                            items
                    ]
            )
            pairs
