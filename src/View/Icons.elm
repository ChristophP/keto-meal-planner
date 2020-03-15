module View.Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


chevronLeft =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M7.05 9.293L6.343 10 12 15.657l1.414-1.414L9.172 10l4.242-4.243L12 4.343z" ] [] ]


chevronRight =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M12.95 10.707l.707-.707L8 4.343 6.586 5.757 10.828 10l-4.242 4.243L8 15.657l4.95-4.95z" ] [] ]


addSolid =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M11 9V5H9v4H5v2h4v4h2v-4h4V9h-4zm-1 11a10 10 0 1 1 0-20 10 10 0 0 1 0 20z" ] [] ]


close =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M10 8.586L2.929 1.515 1.515 2.929 8.586 10l-7.071 7.071 1.414 1.414L10 11.414l7.071 7.071 1.414-1.414L11.414 10l7.071-7.071-1.414-1.414L10 8.586z" ] [] ]


trash =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M6 2l2-2h4l2 2h4v2H2V2h4zM3 6h14l-1 14H4L3 6zm5 2v10h1V8H8zm3 0v10h1V8h-1z" ] [] ]
