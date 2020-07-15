module View exposing
    ( container
    , header
    , notFound
    )

import Element exposing (Element, column, el, layout, link, text)
import Element.Region exposing (heading)



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
    [ el [ heading 1 ] (text "404")
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
