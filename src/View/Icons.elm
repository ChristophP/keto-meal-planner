module View.Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


chevronLeft =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M7.05 9.293L6.343 10 12 15.657l1.414-1.414L9.172 10l4.242-4.243L12 4.343z" ] [] ]


chevronRight =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M12.95 10.707l.707-.707L8 4.343 6.586 5.757 10.828 10l-4.242 4.243L8 15.657l4.95-4.95z" ] [] ]
