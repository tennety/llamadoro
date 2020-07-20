module Palette exposing (color, fromElmColor, heading, scaled, timer)

import Color
import Element exposing (Element)
import Element.Font as Font
import Element.Region
import String.Extra


color =
    { lightest = Element.rgb255 0xF7 0xFA 0xFC
    , lighter = Element.rgb255 0xE2 0xE8 0xF0
    , light = Element.rgb255 0xCB 0xD5 0xE0
    , neutral = Element.rgb255 0xA0 0xAE 0xC0
    , dark = Element.rgb255 0x4A 0x55 0x68
    , darker = Element.rgb255 0x2D 0x37 0x48
    , darkest = Element.rgb255 0x1A 0x20 0x2C

    -- https://coolors.co/587291-69b578-ffec51-de6c83-0ad3ff
    , free = Element.rgb255 105 181 120
    , busy = Element.rgb255 222 108 131
    , copy = Element.rgb255 88 114 145
    , blue = Element.rgb255 10 211 255
    , yellow = Element.rgb255 255 236 81
    }


fromElmColor : Color.Color -> Element.Color
fromElmColor =
    Color.toRgba >> Element.fromRgb


scaled =
    Element.modular 20 1.25 >> round


heading : Int -> List (Element msg) -> Element msg
heading level content =
    Element.paragraph
        ([ Font.regular
         , Font.family [ Font.typeface "Asap" ]
         , Font.color color.darker
         , Element.Region.heading level
         ]
            ++ (case level of
                    1 ->
                        [ Font.size (scaled 4) ]

                    2 ->
                        [ Font.size (scaled 3) ]

                    3 ->
                        [ Font.size (scaled 2) ]

                    _ ->
                        [ Font.size (scaled 1) ]
               )
        )
        content


timer : Element.Color -> String -> Element msg
timer activityColor content =
    let
        removeLeadingZero str =
            if String.startsWith "0" str then
                str |> String.Extra.replaceSlice "  " 0 1

            else
                str
    in
    Element.el
        [ Font.regular
        , Font.family [ Font.typeface "Bungee Shade" ]
        , Font.size (scaled 10)
        , Font.color activityColor
        , Element.width (Element.fill |> Element.minimum 540)
        ]
        (content |> removeLeadingZero |> Element.text)
