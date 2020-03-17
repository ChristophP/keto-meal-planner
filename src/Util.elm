module Util exposing (..)


toFixed : Int -> Float -> String
toFixed digits num =
    let
        factor =
            10 ^ digits

        int =
            round (num * toFloat (10 ^ digits))

        decimals =
            modBy factor int
    in
    String.fromInt (int // factor)
        ++ "."
        ++ String.fromInt
            (if isNaN (toFloat decimals) then
                0

             else
                decimals
            )


toPercentage : Float -> String
toPercentage num =
    toFixed 1 (num * 100) ++ "%"
