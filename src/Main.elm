module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data.Food as Food
import Data.Session as Session exposing (Session)
import Html exposing (Html, a, div, header, li, main_, nav, text, ul)
import Html.Attributes exposing (class, href)
import Json.Decode as JD
import Page.Meals as Meals
import Url exposing (Url)
import Url.Parser as UrlParser
import View.Helpers as VH
import Zondicons as Icons


main : Program JD.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChange
        }



-- MODEL


type Page
    = Meals Meals.Model
    | Foods Session
    | Recipes Session


getSession : Page -> Session
getSession page =
    case page of
        Meals model ->
            model.session

        Foods session ->
            session

        Recipes session ->
            session


type alias Model =
    { page : Page
    , navKey : Nav.Key
    }


init : JD.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        foods =
            JD.decodeValue Food.decoder json
                |> Result.mapError (always "Could not decode food list")

        session =
            Session.init navKey foods
    in
    updateUrl url
        { page = Meals (Meals.init session)
        , navKey = navKey
        }



-- UPDATE


updatePage :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> Model
    -> ( pageModel, Cmd pageMsg )
    -> ( Model, Cmd Msg )
updatePage toPage toMsg model ( pageModel, pageCmd ) =
    ( { model | page = toPage pageModel }, Cmd.map toMsg pageCmd )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChange Url.Url
    | MealsMsg Meals.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChange url ->
            updateUrl url model

        MealsMsg pageMsg ->
            case model.page of
                Meals pageModel ->
                    updatePage Meals MealsMsg model (Meals.update pageMsg pageModel)

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- ROUTING


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    let
        session =
            getSession model.page

        parser =
            UrlParser.oneOf
                [ UrlParser.map ( { model | page = Meals (Meals.init session) }, Cmd.none ) (UrlParser.s "meals")
                , UrlParser.map ( { model | page = Foods session }, Cmd.none ) (UrlParser.s "foods")
                , UrlParser.map ( { model | page = Recipes session }, Cmd.none ) (UrlParser.s "recipes")
                ]
    in
    case UrlParser.parse parser url of
        Nothing ->
            -- redirect to meals page in case of failed url parsing
            ( model, Nav.replaceUrl model.navKey "meals" )

        Just pair ->
            pair



-- VIEW


viewDropdown title =
    div [ class "relative dropdown" ]
        [ div [ class "px-2 uppercase cursor-pointer", Html.Attributes.tabindex 0 ]
            [ text title ]
        , ul
            [ class "absolute items-center justify-between flex-1 bg-white"
            , class "shadow-md"
            ]
            [ li [] [ a [ class "px-2 py-1", href "/foods" ] [ text "Foods" ] ]
            , li [] [ a [ class "px-2 py-1", href "/meals" ] [ text "Meals" ] ]
            , li [] [ a [ class "px-2 py-1", href "/recipes" ] [ text "Recipes" ] ]
            ]
        ]


viewSkeleton : (a -> msg) -> VH.Skeleton a -> Html msg
viewSkeleton toMsg skeleton =
    div [ class "relative w-full h-full mx-auto bg-gray-200 max-w-screen-sm" ] <|
        [ header [ class "sticky top-0 w-full" ]
            [ nav [ class "relative z-10 flex items-center bg-white shadow-md" ]
                [ viewDropdown skeleton.menuTitle
                , div [ class "w-12 ml-auto" ] [ Icons.dotsHorizontalTriple [] ]
                ]
            , div [] <| List.map (Html.map toMsg) skeleton.subHeader
            ]
        , main_ [] <| List.map (Html.map toMsg) skeleton.body
        ]


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Keto Meal Planner"
        [ case model.page of
            Meals pageModel ->
                viewSkeleton MealsMsg (Meals.view pageModel)

            Foods _ ->
                viewSkeleton never { subHeader = [], body = [ text "Coming Soon" ], menuTitle = "Foods" }

            Recipes _ ->
                viewSkeleton never { subHeader = [], body = [ text "Coming Soon" ], menuTitle = "Recipes" }
        ]
