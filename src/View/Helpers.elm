module View.Helpers exposing (dialog)

import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes exposing (class, style, type_)
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


dialog { show, title, content, onClose } =
    div
        (attrList
            [ ( class "fixed inset-0 w-screen h-screen flex items-center justify-center p-4", True )
            , ( style "transform" "translateY(-100vh)", not show )
            , ( style "transform" "none", show )
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
                [ ( class "w-full h-full bg-white rounded-lg z-10 shadow-md", True )
                , ( class "transition-transform duration-500", True )
                , ( style "transform" "translateY(-100vh)", not show )
                ]
            )
            [ div [ class "py-2 border-b border-gray-400 flex items-center justify-center text-xl relative" ]
                [ text title
                , button [ class "absolute w-4 right-0 mr-2", onClick onClose ]
                    [ Icons.close ]
                ]
            , div [] content
            ]
        ]
