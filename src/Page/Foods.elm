module Page.Foods exposing (..)

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


type State
    = Loading
    | ShowList
    | Edit EditState
    | Error


type EditState
    = Editing
    | Submitted


type alias Model =
    { name : String
    , protein : String
    , fat : String
    , carbs : String
    , state : State
    , session : Session
    }


init session =
    { name = ""
    , protein = ""
    , fat = ""
    , carbs = ""
    , state = Loading
    , session = session
    }



-- UPDATE


type Msg
    = FoodNameChanged String
    | FoodProteinChanged String
    | FoodFatChanged String
    | FoodCarbsChanged String
    | DialogCancelled
    | FoodSubmitted
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FoodNameChanged name ->
            ( { model | name = name }, Cmd.none )

        FoodProteinChanged protein ->
            ( { model | protein = protein }, Cmd.none )

        FoodFatChanged fat ->
            ( { model | fat = fat }, Cmd.none )

        FoodCarbsChanged carbs ->
            ( { model | carbs = carbs }, Cmd.none )

        FoodSubmitted ->
            ( model, Cmd.none )

        --case parseFood of
        --Ok food ->
        --Err err -> Debug.todo "Handle error"
        DialogCancelled ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


foodNameInputId : String
foodNameInputId =
    "food-name-input"


view : Model -> VH.Skeleton Msg
view model =
    let
        body =
            case model.state of
                _ ->
                    [ div [ class "px-4 py-2 space-y-2" ]
                        [ VH.inputField
                            [ id foodNameInputId
                            , placeholder "Food name"
                            , onInput FoodNameChanged
                            , value model.name
                            ]
                            []
                        , VH.inputField
                            [ placeholder "Protein"
                            , onInput FoodProteinChanged
                            , value model.protein
                            ]
                            []
                        , VH.inputField
                            [ placeholder "Fat"
                            , onInput FoodFatChanged
                            , value model.fat
                            ]
                            []
                        , VH.inputField
                            [ placeholder "Carbs"
                            , onInput FoodCarbsChanged
                            , value model.carbs
                            ]
                            []
                        ]
                    , VH.dialog
                        { show = False
                        , title = "Pick Food"
                        , content =
                            [ div [ class "flex flex-col h-full" ]
                                []
                            ]
                        , onClose = DialogCancelled
                        }
                    ]
    in
    { subHeader = [], body = body, menuTitle = "Foods" }
