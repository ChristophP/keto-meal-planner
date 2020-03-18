module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Data.Food as Food
import Data.Meal as Meal
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, ol, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import List.Extra as LE
import String.Mark as Mark
import Task
import Util exposing (toFixed, toPercentage)
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
    , selectedFoods : List ( Int, Food.Food )
    , openOverlay : Maybe Int
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
      , openOverlay = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | AddButtonClicked
    | DialogCancelled
    | SearchChanged String
    | FoodSelected Food.Food
    | DeleteFoodClicked Int
    | OverlayClicked Int
    | FoodWeightPicked Int Int
    | FoodWeightEdited String Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

        AddButtonClicked ->
            ( { model
                | showFoods = True
                , openOverlay = Maybe.map ((+) 1) model.openOverlay
              }
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
                , openOverlay = Nothing
              }
            , scrollToBottom contentBodyId
            )

        DeleteFoodClicked index ->
            let
                newFoods =
                    LE.removeIfIndex ((==) index) model.selectedFoods
            in
            ( { model
                | selectedFoods = newFoods
                , openOverlay = Nothing
              }
            , Cmd.none
            )

        OverlayClicked index ->
            let
                newOpenOverlay =
                    case model.openOverlay of
                        Just openIndex ->
                            if openIndex == index then
                                Nothing

                            else
                                Just index

                        Nothing ->
                            Just index
            in
            ( { model | openOverlay = newOpenOverlay }, Cmd.none )

        FoodWeightPicked weight index ->
            let
                newSelectedFoods =
                    LE.updateIfIndex ((==) index)
                        (Tuple.mapFirst (\_ -> weight))
                        model.selectedFoods
            in
            ( { model | selectedFoods = newSelectedFoods }, Cmd.none )

        FoodWeightEdited inputValue index ->
            case String.toInt inputValue of
                Just weight ->
                    let
                        newSelectedFoods =
                            LE.updateIfIndex ((==) index)
                                (Tuple.mapFirst (\_ -> weight))
                                model.selectedFoods
                    in
                    ( { model | selectedFoods = newSelectedFoods }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


scrollToBottom : String -> Cmd Msg
scrollToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


foodSearchId : String
foodSearchId =
    "food-search"


contentBodyId : String
contentBodyId =
    "content-body"


viewNutrientPctg : Float -> Food.Food -> String
viewNutrientPctg num food =
    toFixed 1 (num / Food.totalGrams food * 100) ++ "%"


viewMealGrams : (Food.Food -> Float) -> List ( Int, Food.Food ) -> Html Msg
viewMealGrams getter foods =
    let
        totalGrams =
            List.map (\( grams, food ) -> toFloat grams * getter food / 100) foods
                |> List.sum
    in
    span
        [ class "font-medium text-indigo-700"
        ]
        [ text <| toFixed 1 totalGrams ++ "g" ]


viewMealPercentage : (Food.Food -> Float) -> Float -> List ( Int, Food.Food ) -> Html Msg
viewMealPercentage getter threshold foods =
    let
        totalGrams =
            List.map (\( grams, food ) -> toFloat grams * getter food / 100) foods
                |> List.sum

        totalMealGrams =
            List.map
                (\( grams, food ) ->
                    (toFloat grams
                        * food.protein
                        + toFloat grams
                        * food.fat
                        + toFloat grams
                        * food.carbs
                    )
                        / 100
                )
                foods
                |> List.sum

        ratio =
            totalGrams / totalMealGrams

        ratioGood =
            abs (ratio - threshold) < 0.05
    in
    span
        [ class "font-medium"
        , VH.attrIf ratioGood <| class "text-indigo-700"
        , VH.attrIf (not ratioGood) <| class "text-red-500"
        ]
        [ text <| toPercentage ratio ]


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
        [ div [ class "flex flex-col w-full h-full mx-auto bg-gray-200 max-w-screen-sm" ] <|
            case model.foods of
                Ok foods ->
                    [ VH.slider
                        { items = List.map Meal.toString Meal.meals
                        , onBack = Decrement
                        , onNext = Increment
                        , index = model.count
                        }
                    , div
                        [ class "flex-1 overflow-y-auto smooth-scroll"
                        , id contentBodyId
                        ]
                        [ viewTotalNutrientsHeader model mealPctg
                        , ol [ class "mt-4 text-2xl text-center" ] <|
                            List.indexedMap
                                (viewFoodItem model.openOverlay)
                                model.selectedFoods
                        ]
                    , button
                        [ class "bottom-0 w-16 h-16 mx-auto mb-2 bg-white rounded-full"
                        , class "text-indigo-600 shadow-lg hover:text-indigo-800"
                        , onClick AddButtonClicked
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
                                    , onClick <| FoodSelected item
                                    ]
                                    (Mark.mark searchTerm item.name)
                            )
                            items
                    ]
            )
            pairs


viewFoodItem : Maybe Int -> Int -> ( Int, Food.Food ) -> Html Msg
viewFoodItem maybeOpenIndex index ( grams, food ) =
    let
        isOpen =
            case maybeOpenIndex of
                Just openIndex ->
                    openIndex == index

                Nothing ->
                    False
    in
    li [ class "relative flex flex-col pr-8 mt-2 overflow-x-hidden bg-white shadow" ]
        [ div
            [ class "relative flex items-center justify-center"
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
            , onOverlayClick = OverlayClicked index
            , onDelete = DeleteFoodClicked index
            , grams = grams
            , index = index
            }
        ]


viewFoodOverlay :
    { open : Bool
    , onOverlayClick : Msg
    , onDelete : Msg
    , grams : Int
    , index : Int
    }
    -> Html Msg
viewFoodOverlay { open, onOverlayClick, onDelete, grams, index } =
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
                    [ Icons.chevronLeft ]
                ]
            , viewGramPicker grams index
            , button
                [ class "w-6 mr-2 text-gray-400"
                , class "cursor-pointer hover:text-gray-600"
                , onClick onDelete
                ]
                [ Icons.trash ]
            ]
        ]


viewGramPicker : Int -> Int -> Html Msg
viewGramPicker grams index =
    let
        buttonClasses =
            "w-12 bg-gray-500 hover:bg-gray-700 active:bg-gray-700"
    in
    div [ class "flex px-2 shadow-md" ]
        [ button
            [ type_ "button"
            , class buttonClasses
            , class "rounded-l-md"
            , onClick <| FoodWeightPicked (grams - 5) index
            , disabled (grams - 5 < 0)
            ]
            [ text "-" ]
        , input
            [ class "w-16 text-center text-gray-700"
            , value (String.fromInt grams)
            , type_ "text"
            , onInput <| \inputValue -> FoodWeightEdited inputValue index
            ]
            []
        , button
            [ class buttonClasses
            , class "rounded-r-md"
            , onClick <| FoodWeightPicked (grams + 5) index
            ]
            [ text "+" ]
        ]


viewTotalNutrientsHeader : Model -> Float -> Html Msg
viewTotalNutrientsHeader model mealPctg =
    let
        mealCalories =
            totalAllowedCalories * mealPctg

        proteinTarget =
            mealCalories * targetNutritionRatio.protein / caloriesPerGram.protein

        fatTarget =
            mealCalories * targetNutritionRatio.fat / caloriesPerGram.fat

        carbsTarget =
            mealCalories * targetNutritionRatio.carbs / caloriesPerGram.carbs
    in
    div [ class "mt-2 text-2xl text-center bg-white" ]
        [ span [ class "text-sm tracking-widest uppercase" ] [ text "Target calories" ]
        , div [ class "flex pr-8" ]
            [ div [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                [ span [] [ text "Protein" ]
                , span []
                    [ viewMealGrams .protein model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 proteinTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage .protein targetNutritionRatio.protein model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.protein)
                    ]
                ]
            , div [ class "flex flex-col flex-1 p-2 text-sm border-r border-black" ]
                [ span [ class "text-sm" ] [ text "Fat" ]
                , span []
                    [ viewMealGrams .fat model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 fatTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage .fat targetNutritionRatio.fat model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.fat)
                    ]
                ]
            , div [ class "flex flex-col flex-1 p-2 text-sm" ]
                [ span [] [ text "Carbs" ]
                , span []
                    [ viewMealGrams .carbs model.selectedFoods
                    , text " / "
                    , text <| toFixed 2 carbsTarget ++ "g"
                    ]
                , span []
                    [ viewMealPercentage .carbs targetNutritionRatio.carbs model.selectedFoods
                    , text " / "
                    , text (toPercentage targetNutritionRatio.carbs)
                    ]
                ]
            ]
        ]
