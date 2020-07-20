module View exposing
    ( container
    , header
    , heading
    , notFound
    , timer
    )

import Element exposing (Element, column, el, fill, link, minimum, paragraph, text, width)
import Element.Font as Font
import Element.Region as Region
import Palette
import String.Extra



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


timer : Element.Color -> String -> Element msg
timer activityColor content =
    let
        removeLeadingZero str =
            if String.startsWith "0" str then
                str |> String.Extra.replaceSlice "  " 0 1

            else
                str
    in
    el
        [ Font.regular
        , Font.family [ Palette.fontFamily.display ]
        , Font.size (Palette.scaled 10)
        , Font.color activityColor
        , width (fill |> minimum 540)
        ]
        (content |> removeLeadingZero |> text)


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
