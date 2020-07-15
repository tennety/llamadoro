module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, column, el, layout, link, text)
import Element.Region exposing (heading)
import Html exposing (Html)
import Route exposing (Route(..))
import Url exposing (Url)
import View


type alias Flags =
    {}



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    }


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
      BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


project : { description : String, title : String }
project =
    { title = "Llamadoro"
    , description = """
        Improve your fitness while you work. With llamas.
      """
    }


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    layout [] <|
        column []
            [ View.header
                [ link [] { label = text "Home", url = "/" }
                , link [] { label = text "Settings", url = "/settings" }
                , link [] { label = text "Exercises", url = "/exercises" }
                ]
            , View.container <|
                case model.route of
                    Home ->
                        viewHome model

                    Settings ->
                        viewSettings model

                    Exercises ->
                        viewExercises model

                    NotFound ->
                        View.notFound
            ]


viewHome : Model -> List (Element Msg)
viewHome model =
    [ el [ heading 1 ] (Element.text "Llamadoro")
    , el [] (Element.text project.description)
    ]


viewSettings : Model -> List (Element Msg)
viewSettings model =
    [ el [ heading 1 ] (Element.text "Llamadoro - Settings")
    , el [] (Element.text project.description)
    ]


viewExercises : Model -> List (Element Msg)
viewExercises model =
    [ el [ heading 1 ] (Element.text "Llamadoro - Exercises")
    , el [] (Element.text project.description)
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        UserClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
