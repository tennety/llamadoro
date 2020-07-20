module Palette exposing (color, fontFamily, fromElmColor, scaled)

import Color
import Element exposing (fromRgb, rgb255)
import Element.Font as Font



-- https://coolors.co/587291-69b578-ffec51-de6c83-0ad3ff


color =
    { free = rgb255 105 181 120
    , busy = rgb255 222 108 131
    , copy = rgb255 88 114 145
    , blue = rgb255 10 211 255
    , yellow = rgb255 255 236 81
    }


fontFamily =
    { title = Font.typeface "Asap, monospace"
    , display = Font.typeface "Bungee Shade"
    }


fromElmColor : Color.Color -> Element.Color
fromElmColor =
    Color.toRgba >> fromRgb


scaled =
    Element.modular 20 1.25 >> round
