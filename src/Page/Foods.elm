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
    , state = Editing
    , session = session
    }



-- UPDATE


type Msg
    = FoodNameChanged String
    | DialogCancelled
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FoodNameChanged name ->
            ( { model | name = name }, Cmd.none )

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
    { subHeader = []
    , body =
        case model.session.foods of
            Ok foods ->
                [ div []
                    [ VH.inputField
                        [ id foodNameInputId
                        , placeholder "Search for Food"
                        , onInput FoodNameChanged
                        , value model.name
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

            Err err ->
                [ text "Foods lists could not be parsed" ]
    , menuTitle = "Foods"
    }
