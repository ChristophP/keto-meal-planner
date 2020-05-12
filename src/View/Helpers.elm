module View.Helpers exposing
    ( Skeleton
    , attrIf
    , dialog
    , externalLink
    , inputField
    , scrollToBottom
    , slider
    , viewIf
    )

import Browser.Dom as Dom
import Html exposing (Attribute, Html, a, button, div, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, rel, style, target, title)
import Html.Events exposing (onClick)
import Task
import Zondicons as Icons


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf show attr =
    if show then
        attr

    else
        classList []


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf show toHtml =
    if show then
        toHtml ()

    else
        text ""


inputField : List (Attribute msg) -> List (Html msg) -> Html msg
inputField attr =
    input
        (class "w-full p-2 shadow focus:shadow-outline" :: attr)


externalLink :
    { href : String
    , title : Maybe String
    , children : List (Html msg)
    , attr : List (Attribute msg)
    }
    -> Html msg
externalLink props =
    a
        ([ href props.href
         , title (Maybe.withDefault "" props.title)
         , target "_blank"
         , rel "noopener noreferrer"
         ]
            ++ props.attr
        )
        props.children


dialog :
    { show : Bool
    , title : String
    , content : List (Html msg)
    , onClose : msg
    }
    -> Html msg
dialog { show, title, content, onClose } =
    div
        [ class "fixed inset-0 z-30 flex items-center justify-center w-screen h-screen p-4"
        , attrIf (not show) <| style "transform" "translateY(-100vh)"
        ]
        [ div
            [ class "absolute inset-0 bg-black opacity-50"
            , class "transition-opacity duration-500"
            , onClick onClose
            , attrIf (not show) <| style "opacity" "0"
            ]
            []
        , div
            [ class "relative flex flex-col w-full h-full max-w-screen-sm"
            , class "bg-white rounded-lg shadow-md transition-transform duration-500"
            , attrIf (not show) <| style "transform" "translateY(-100vh)"
            ]
            [ div [ class "relative flex items-center justify-center py-2 text-xl border-b border-gray-400" ]
                [ text title
                , button [ class "absolute right-0 w-4 mr-2", onClick onClose ]
                    [ Icons.close [] ]
                ]
            , div [ class "flex-1 overflow-hidden" ] content
            ]
        ]


slider :
    { onBack : msg
    , onNext : msg
    , index : Int
    , items : List String
    }
    -> Html msg
slider { onBack, onNext, index, items } =
    let
        buttonClasses =
            class "w-16 text-indigo-600 hover:text-indigo-800"
    in
    div [ class "flex items-center justify-between w-full text-2xl text-center bg-white shadow-md" ]
        [ button
            [ buttonClasses
            , onClick onBack
            , disabled (index <= 0)
            ]
            [ Icons.cheveronLeft [] ]
        , div [ class "flex-1 overflow-hidden" ]
            [ ul
                [ class "flex items-center flex-full transition-tranform duration-500"
                , style "transform" ("translateX(-" ++ String.fromInt (index * 100) ++ "%)")
                ]
              <|
                List.map
                    (\item ->
                        li [ class "px-2 text-center flex-full" ] [ text item ]
                    )
                    items
            ]
        , button
            [ buttonClasses
            , onClick onNext
            , disabled (index >= List.length items - 1)
            ]
            [ Icons.cheveronRight [] ]
        ]


scrollToBottom : (Result Dom.Error () -> msg) -> String -> Cmd msg
scrollToBottom toMsg id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt toMsg


type alias Skeleton msg =
    { subHeader : List (Html msg)
    , body : List (Html msg)
    , menuTitle : String
    }
