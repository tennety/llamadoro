module View exposing
    ( container
    , header
    , heading
    , notFound
    , timer
    )

import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, link, minimum, paddingXY, paragraph, row, shrink, spacingXY, text, width)
import Element.Font as Font
import Element.Region as Region
import Palette



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
        [ Font.family [ Palette.fontFamily.display ]
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
         , Font.family [ Palette.fontFamily.title ]
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
