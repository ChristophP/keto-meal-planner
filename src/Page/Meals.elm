module Page.Meals exposing (..)

import Browser.Dom as Dom
import Data.Food as Food
import Data.Meal as Meal
import Data.Session exposing (Session)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, ol, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as LE
import String.Mark as Mark
import Svg.Attributes
import Task
import Util exposing (toFixed, toPercentage)
import View.Helpers as VH
import Zondicons as Icons



-- MODEL


totalAllowedCalories =
    800


targetNutritionRatio =
    { protein = 0.09, fat = 0.79, carbs = 0.12 }


type alias Model =
    { count : Int
    , showFoods : Bool
    , searchTerm : String
    , selectedFoods : List ( Int, Food.Food )
    , openOverlay : Maybe String
    , session : Session
    }


init session =
    { count = 0
    , showFoods = False
    , searchTerm = ""
    , selectedFoods = []
    , openOverlay = Nothing
    , session = session
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | AddButtonClicked
    | DialogCancelled
    | SearchChanged String
    | FoodSelected Food.Food
    | DeleteFoodClicked String
    | OverlayClicked String
    | FoodWeightPicked Int String
    | FoodWeightEdited String String
    | NoOp


updateOpenOverlay : String -> Maybe String -> Maybe String
updateOpenOverlay name openOverlay =
    case openOverlay of
        -- already open
        Just openOverlayName ->
            if name == openOverlayName then
                Nothing

            else
                Just name

        -- none open yet
        Nothing ->
            Just name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        AddButtonClicked ->
            ( { model | showFoods = True }
            , Task.attempt (always NoOp) (Dom.focus foodSearchId)
            )

        DialogCancelled ->
            ( { model | showFoods = False }, Cmd.none )

        SearchChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        FoodSelected food ->
            ( { model
                | selectedFoods = model.selectedFoods ++ [ ( 10, food ) ]
                , showFoods = False
                , searchTerm = ""
                , openOverlay = updateOpenOverlay food.name model.openOverlay
              }
            , VH.scrollToBottom (always NoOp) contentBodyId
            )

        DeleteFoodClicked name ->
            let
                newFoods =
                    List.filter (Tuple.second >> .name >> (/=) name) model.selectedFoods
            in
            ( { model
                | selectedFoods = newFoods
                , openOverlay = Nothing
              }
            , Cmd.none
            )

        OverlayClicked foodName ->
            ( { model | openOverlay = updateOpenOverlay foodName model.openOverlay }
            , Cmd.none
            )

        FoodWeightPicked weight name ->
            let
                newSelectedFoods =
                    LE.updateIf (Tuple.second >> .name >> (==) name)
                        (Tuple.mapFirst (\_ -> weight))
                        model.selectedFoods
            in
            ( { model | selectedFoods = newSelectedFoods }, Cmd.none )

        FoodWeightEdited inputValue name ->
            case String.toInt inputValue of
                Just weight ->
                    let
                        newSelectedFoods =
                            LE.updateIf (Tuple.second >> .name >> (==) name)
                                (Tuple.mapFirst (\_ -> weight))
                                model.selectedFoods
                    in
                    ( { model | selectedFoods = newSelectedFoods }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


foodSearchId : String
foodSearchId =
    "food-search"


contentBodyId : String
contentBodyId =
    "content-body"


viewNutrientPctg : Float -> Food.Food -> String
viewNutrientPctg num food =
    toFixed 1 (num / Food.totalNutrientWeightPer100grams food * 100) ++ "%"


viewMealGrams : Food.Nutrient -> List ( Int, Food.Food ) -> Html Msg
viewMealGrams nutrient foods =
    let
        totalGrams =
            List.map (\( grams, food ) -> Food.getNutrientGrams nutrient (toFloat grams) food) foods
                |> List.sum
    in
    span
        [ class "font-medium text-indigo-700" ]
        [ text <| toFixed 1 totalGrams ++ "g" ]


viewMealPercentage : Food.Nutrient -> Float -> List ( Int, Food.Food ) -> Html Msg
viewMealPercentage nutrient threshold foods =
    let
        caloriesFromNutrient =
            List.map (\( grams, food ) -> Food.getNutrientCalories nutrient (toFloat grams) food) foods
                |> List.sum

        mealCalories =
            List.map
                (\( grams, food ) -> Food.getCalories (toFloat grams) food)
                foods
                |> List.sum

        ratio =
            caloriesFromNutrient / mealCalories

        ratioGood =
            abs (ratio - threshold) < 0.05
    in
    span
        [ class "font-medium"
        , VH.attrIf ratioGood <| class "text-indigo-700"
        , VH.attrIf (not ratioGood) <| class "text-red-500"
        ]
        [ text <| toPercentage ratio ]


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
    div [ class "flex-1 overflow-y-auto v-gap-4" ] <|
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
                                    , onClick <| FoodSelected item
                                    ]
                                    (Mark.mark searchTerm item.name)
                            )
                            items
                    ]
            )
            pairs


viewFoodItem : Maybe String -> ( Int, Food.Food ) -> Html Msg
viewFoodItem maybeOpenFood ( grams, food ) =
    let
        isOpen =
            case maybeOpenFood of
                Just foodName ->
                    foodName == food.name

                Nothing ->
                    False
    in
    li [ class "relative flex flex-col pr-8 mt-2 overflow-x-hidden bg-white shadow" ]
        [ div
            [ class "flex items-center justify-center"
            , class "text-sm font-semibold leading-relaxed"
            ]
            [ span [ class "text-indigo-700 " ] [ text food.name ]
            ]
        , div [ class "flex pb-1" ]
            [ div [ class "flex flex-col flex-1 px-1 text-sm" ]
                [ span [] [ text "Protein" ]
                , span [] [ text <| toFixed 2 food.protein, text "g" ]
                , span [] [ text (viewNutrientPctg food.protein food) ]
                ]
            , div [ class "flex flex-col flex-1 px-1 text-sm" ]
                [ span [ class "text-sm" ] [ text "Fat" ]
                , span [] [ text <| toFixed 2 food.fat, text "g" ]
                , span [] [ text (viewNutrientPctg food.fat food) ]
                ]
            , div [ class "flex flex-col flex-1 px-1 text-sm" ]
                [ span [] [ text "Carbs" ]
                , span [] [ text <| toFixed 2 food.carbs, text "g" ]
                , span [] [ text (viewNutrientPctg food.carbs food) ]
                ]
            ]
        , viewFoodOverlay
            { open = isOpen
            , onOverlayClick = OverlayClicked food.name
            , onDelete = DeleteFoodClicked food.name
            , grams = grams
            , name = food.name
            }
        ]


viewFoodOverlay :
    { open : Bool
    , onOverlayClick : Msg
    , onDelete : Msg
    , grams : Int
    , name : String
    }
    -> Html Msg
viewFoodOverlay { open, onOverlayClick, onDelete, grams, name } =
    div
        [ class "absolute flex-1 w-full h-full"
        , class "overflow-hidden text-white rounded-l"
        , class "transition-transform duration-500"
        , VH.attrIf (not open) <| style "transform" "translateX(90%)"
        ]
        [ div [ class "absolute inset-0 bg-indigo-700 opacity-75" ] []
        , div [ class "absolute inset-0 flex items-center justify-between" ]
            [ button [ class "w-6 h-full hover:bg-indigo-800", onClick onOverlayClick ]
                [ div
                    [ class "transform"
                    , classList [ ( "rotate-180", open ) ]
                    ]
                    [ Icons.cheveronLeft [] ]
                ]
            , viewGramPicker grams name
            , button
                [ class "w-6 mr-2 text-gray-400"
                , class "cursor-pointer hover:text-gray-600"
                , onClick onDelete
                ]
                [ Icons.trash [] ]
            ]
        ]


viewGramPicker : Int -> String -> Html Msg
viewGramPicker grams name =
    let
        buttonClasses =
            "w-12 bg-gray-500 hover:bg-gray-700 active:bg-gray-700"
    in
    div [ class "flex shadow-md rounded-r-md rounded-l-md" ]
        [ button
            [ type_ "button"
            , class buttonClasses
            , class "rounded-l-md"
            , onClick <| FoodWeightPicked (grams - 5) name
            , disabled (grams - 5 < 0)
            ]
            [ text "-" ]
        , input
            [ class "w-16 text-center text-gray-700 rounded-none"
            , value (String.fromInt grams)
            , type_ "text"
            , onInput <| \inputValue -> FoodWeightEdited inputValue name
            ]
            []
        , button
            [ class buttonClasses
            , class "rounded-r-md"
            , onClick <| FoodWeightPicked (grams + 5) name
            ]
            [ text "+" ]
        ]


viewTotalNutrientsHeader : Model -> Float -> Html Msg
viewTotalNutrientsHeader model mealPctg =
    let
        mealCalories =
            totalAllowedCalories * mealPctg

        proteinTarget =
            mealCalories * targetNutritionRatio.protein / Food.caloriesPerGram.protein

        fatTarget =
            mealCalories * targetNutritionRatio.fat / Food.caloriesPerGram.fat

        carbsTarget =
            mealCalories * targetNutritionRatio.carbs / Food.caloriesPerGram.carbs
    in
    div [ class "mt-2 text-2xl text-center bg-white" ]
        [ span [ class "text-sm tracking-widest uppercase" ] [ text "Target calories" ]
        , div [ class "flex pr-8 divide-x divide-gray-600" ]
            [ div [ class "flex flex-col flex-1 px-2 my-2 text-sm" ]
                [ span [] [ text "Protein" ]
                , span []
                    [ viewMealGrams Food.Protein model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 proteinTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage Food.Protein targetNutritionRatio.protein model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.protein)
                    ]
                ]
            , div [ class "flex flex-col flex-1 px-2 my-2 text-sm" ]
                [ span [ class "text-sm" ] [ text "Fat" ]
                , span []
                    [ viewMealGrams Food.Fat model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 fatTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage Food.Fat targetNutritionRatio.fat model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.fat)
                    ]
                ]
            , div [ class "flex flex-col flex-1 px-2 my-2 text-sm" ]
                [ span [] [ text "Carbs" ]
                , span []
                    [ viewMealGrams Food.Carbs model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 carbsTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage Food.Carbs targetNutritionRatio.carbs model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.carbs)
                    ]
                ]
            ]
        ]


view : Model -> VH.Skeleton Msg
view model =
    { subHeader =
        [ VH.slider
            { items = List.map Meal.toString Meal.meals
            , onBack = Decrement
            , onNext = Increment
            , index = model.count
            }
        ]
    , body =
        let
            mealPctg =
                case LE.getAt model.count Meal.meals of
                    Just meal ->
                        Meal.toPercentage meal

                    Nothing ->
                        -- should never happen
                        0
        in
        [ div
            [ class "flex-1 mb-16 smooth-scroll"
            , id contentBodyId
            ]
            [ viewTotalNutrientsHeader model mealPctg
            , ol [ class "mt-4 text-2xl text-center" ] <|
                List.map
                    (viewFoodItem model.openOverlay)
                    model.selectedFoods
            , button
                [ class "fixed bottom-0 left-0 right-0 block w-16 h-16 mx-auto mb-2 bg-white rounded-full"
                , class "text-indigo-600 hover:text-indigo-800"
                , class "rounded-full shadow-lg"
                , onClick AddButtonClicked
                ]
                [ Icons.addSolid [] ]
            ]
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
                    , viewFoodsList model.searchTerm model.session.foods
                    ]
                ]
            , onClose = DialogCancelled
            }
        ]
    , menuTitle = "Meals"
    }
