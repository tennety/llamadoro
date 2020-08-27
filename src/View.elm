module View exposing
    ( container
    , header
    , heading
    , notFound
    , timer
    , workCountLlama
    )

import Color
import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, height, html, link, moveUp, paddingXY, paragraph, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Gen.Graphics as Graphics
import Palette
import Svg.Attributes as SA



-- HEADER


header : List (Element msg) -> Element msg
header items =
    Element.row
        []
    <|
        [ link []
            { url = "/"
            , label = text "Llamadoro"
            }
        ]
            ++ items



-- CONTAINER


container : List (Element msg) -> Element msg
container content =
    column
        []
        content



-- DEFAULT PAGES


notFound : List (Element msg)
notFound =
    [ el [ Region.heading 1 ] (text "404")
    , el [] (text "Sorry, we could not find this page.")
    , el
        []
        (link []
            { url = "/"
            , label = text "Home"
            }
        )
    ]



-- MISC


timer : Element.Color -> ( Int, Int ) -> Element msg
timer activityColor ( mins, secs ) =
    row
        [ Font.family Palette.fontFamily.display
        , Font.size (Palette.scaled 8)
        , Font.medium
        , Font.color activityColor
        , centerX
        , width fill
        , Region.description <| String.fromInt mins ++ " minutes remaining."
        ]
        [ el
            [ width (fillPortion 1)
            , Font.alignRight
            ]
            (mins |> String.fromInt |> String.padLeft 2 '0' |> text)
        , el
            [ width shrink
            , paddingXY 5 0
            , Font.size (Palette.scaled 7)
            , Font.center
            , alignTop
            ]
            (text ":")
        , el
            [ width (fillPortion 1)
            , Font.alignLeft
            ]
            (secs |> String.fromInt |> String.padLeft 2 '0' |> text)
        ]


heading : Int -> List (Element msg) -> Element msg
heading level content =
    paragraph
        ([ Font.regular
         , Font.family Palette.fontFamily.title
         , Font.color Palette.color.copy
         , Region.heading level
         ]
            ++ (case level of
                    1 ->
                        [ Font.size (Palette.scaled 4) ]

                    2 ->
                        [ Font.size (Palette.scaled 3) ]

                    3 ->
                        [ Font.size (Palette.scaled 2) ]

                    _ ->
                        [ Font.size (Palette.scaled 1) ]
               )
        )
        content


workCountLlama : Int -> Element msg
workCountLlama count =
    let
        countText =
            String.fromInt count
    in
    row
        [ centerX
        , spacing 5
        , paddingXY 12 5
        , moveUp (Basics.toFloat <| Palette.scaled 2)
        , Font.family Palette.fontFamily.title
        , Font.color Palette.color.busy
        , Font.size (Palette.scaled 1)
        , Font.center
        , Font.light
        , Border.rounded (Palette.scaled 2)
        , Background.color Palette.color.copy
        ]
        [ el [ centerX ]
            (html <|
                Graphics.workLlama
                    [ SA.width "40px"
                    , SA.height "40px"
                    , SA.fill (Color.white |> Color.toCssString)
                    ]
            )
        , el
            [ centerX
            , paddingXY 10 5
            , Border.rounded (Palette.scaled 3)
            , Background.color (Color.white |> Palette.fromElmColor)
            ]
            (text countText)
        ]
