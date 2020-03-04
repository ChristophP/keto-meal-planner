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
    String.fromInt (int // factor) ++ "." ++ String.fromInt decimals
