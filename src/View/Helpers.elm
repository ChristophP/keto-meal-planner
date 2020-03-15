module View.Helpers exposing (dialog, inputField, slider)

import Html exposing (Attribute, Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (class, disabled, style, type_)
import Html.Events exposing (onClick)
import View.Icons as Icons


attrList : List ( Attribute msg, Bool ) -> List (Attribute msg)
attrList =
    List.filterMap
        (\( attr, include ) ->
            if include then
                Just attr

            else
                Nothing
        )


inputField attr =
    input
        (class "w-full p-2 shadow focus:shadow-outline" :: attr)


dialog :
    { show : Bool
    , title : String
    , content : List (Html msg)
    , onClose : msg
    }
    -> Html msg
dialog { show, title, content, onClose } =
    div
        (attrList
            [ ( class "fixed inset-0 flex items-center justify-center w-screen h-screen p-4", True )
            , ( style "transform" "translateY(-100vh)", not show )
            ]
        )
        [ div
            (attrList
                [ ( class "absolute inset-0 bg-black opacity-50", True )
                , ( class "transition-opacity duration-500", True )
                , ( onClick onClose, True )
                , ( style "opacity" "0", not show )
                ]
            )
            []
        , div
            (attrList
                [ ( class "z-10 flex flex-col w-full h-full bg-white rounded-lg shadow-md", True )
                , ( class "transition-transform duration-500", True )
                , ( style "transform" "translateY(-100vh)", not show )
                ]
            )
            [ div [ class "relative flex items-center justify-center py-2 text-xl border-b border-gray-400" ]
                [ text title
                , button [ class "absolute right-0 w-4 mr-2", onClick onClose ]
                    [ Icons.close ]
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
            [ Icons.chevronLeft ]
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
            [ Icons.chevronRight ]
        ]
