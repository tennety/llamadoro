module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, layout, link, text)
import Element.Font as Font exposing (size)
import Element.Region exposing (heading)
import Html exposing (Html)
import Palette
import Route exposing (Route(..))
import Time
import TypedTime exposing (TypedTime)
import Url exposing (Url)
import View


type alias Flags =
    {}


type Stage
    = Work
    | Break -- exercise



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , timeRemaining : TypedTime
    , currentStage : Stage
    }


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | Tick Time.Posix


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
    layout [ centerX ] <|
        column [ centerX ] <|
            case model.route of
                Home ->
                    viewHome model

                Settings ->
                    viewSettings model

                Exercises ->
                    viewExercises model

                NotFound ->
                    View.notFound


viewHome : Model -> List (Element Msg)
viewHome model =
    [ el [ centerX, heading 1 ] (text "Llamadoro")
    , el [ Font.size (Palette.scaled 10), Font.center ] (model.timeRemaining |> TypedTime.toString TypedTime.Seconds |> text)
    ]


viewSettings : Model -> List (Element Msg)
viewSettings model =
    [ el [ heading 1 ] (text "Llamadoro - Settings")
    , el [] (text project.description)
    ]


viewExercises : Model -> List (Element Msg)
viewExercises model =
    [ el [ heading 1 ] (text "Llamadoro - Exercises")
    , el [] (text project.description)
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

        Tick _ ->
            ( { model | timeRemaining = TypedTime.sub model.timeRemaining (TypedTime.seconds 1) }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , timeRemaining = TypedTime.minutes 25.0
      , currentStage = Work
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Time.every 1000 Tick
