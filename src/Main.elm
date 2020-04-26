module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Data.Food as Food
import Data.Session as Session exposing (Session)
import Html exposing (Html, a, div, header, li, main_, nav, text, ul)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
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


isMealsPage : Page -> Bool
isMealsPage page =
    case page of
        Meals _ ->
            True

        _ ->
            False


isFoodsPage : Page -> Bool
isFoodsPage page =
    case page of
        Foods _ ->
            True

        _ ->
            False


isRecipesPage : Page -> Bool
isRecipesPage page =
    case page of
        Recipes _ ->
            True

        _ ->
            False


getSession : Page -> Session
getSession page =
    case page of
        Meals model ->
            model.session

        Foods session ->
            session

        Recipes session ->
            session


updateSession : (Session -> Session) -> Page -> Page
updateSession func page =
    case page of
        Meals pageModel ->
            Meals { pageModel | session = func pageModel.session }

        Foods session ->
            Foods (func session)

        Recipes session ->
            Recipes (func session)


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


closeNav : Model -> Model
closeNav model =
    let
        newPage =
            updateSession (\session -> { session | navOpen = False }) model.page
    in
    { model | page = newPage }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChange Url.Url
    | NavToogle
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
                |> Tuple.mapFirst closeNav

        NavToogle ->
            let
                newPage =
                    updateSession (\session -> { session | navOpen = not session.navOpen })
                        model.page
            in
            ( { model | page = newPage }, Cmd.none )

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


viewDropdown : String -> Html Msg
viewDropdown title =
    div [ class "relative tracking-widest text-bg-black" ]
        [ div
            [ class "px-4 uppercase cursor-pointer"
            , Html.Attributes.tabindex 0
            , onClick NavToogle
            ]
            [ text title ]
        ]


viewNav : Model -> Html Msg
viewNav { page } =
    let
        session =
            getSession page

        viewItem active name link =
            li []
                [ a
                    [ href link
                    , class "block w-full px-4 py-1 my-2 font-bold tracking-wide"
                    , if active then
                        class "bg-indigo-500"

                      else
                        class ""
                    ]
                    [ text name ]
                ]
    in
    div
        [ class "fixed inset-0 w-full h-full mx-auto shadow-md app-width"
        , if session.navOpen then
            style "transform" "none"

          else
            style "transform" "translateX(-100vw)"
        ]
        [ div
            [ class "h-full bg-black"
            , class "transition-opacity duration-200 "
            , if session.navOpen then
                style "opacity" "0.25"

              else
                style "opacity" "0"
            , onClick NavToogle
            ]
            []
        , nav
            [ class "absolute inset-y-0 items-center justify-between w-2/3 pt-10 bg-white"
            , class "text-white bg-indigo-700"
            , class "shadow-md transition transition-transform duration-200"
            , if session.navOpen then
                style "transform" "none"

              else
                style "transform" "translateX(-100vw)"
            ]
            [ ul
                [ class "w-full"
                ]
                [ viewItem (isMealsPage page) "Meals" "/meals"
                , viewItem (isFoodsPage page) "Foods" "/foods"
                , viewItem (isRecipesPage page) "Recipes" "/recipes"
                ]
            ]
        ]


viewSkeleton : (a -> Msg) -> VH.Skeleton a -> Model -> Html Msg
viewSkeleton toMsg skeleton model =
    div [ class "relative w-full h-full mx-auto bg-gray-200 max-w-screen-sm" ] <|
        [ header [ class "sticky top-0 w-full" ]
            [ div [ class "relative flex items-center h-12 text-white bg-indigo-700 shadow-md z-12" ]
                [ viewDropdown skeleton.menuTitle
                , div [ class "w-8 h-8 mr-2 ml-auautoto" ] [ Icons.dotsHorizontalTriple [] ]
                ]
            , div [] <| List.map (Html.map toMsg) skeleton.subHeader
            ]
        , main_ [] <| List.map (Html.map toMsg) skeleton.body
        , viewNav model
        ]


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Keto Meal Planner"
        [ case model.page of
            Meals pageModel ->
                viewSkeleton MealsMsg (Meals.view pageModel) model

            Foods _ ->
                viewSkeleton identity { subHeader = [], body = [ div [ class "flex items-center justify-center h-64" ] [ text "Coming Soon" ] ], menuTitle = "Foods" } model

            Recipes _ ->
                viewSkeleton identity { subHeader = [], body = [ div [ class "flex items-center justify-center h-64" ] [ text "Coming Soon" ] ], menuTitle = "Recipes" } model
        ]
