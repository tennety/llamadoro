module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, layout, paddingXY, text)
import Element.Region exposing (heading)
import Html exposing (Html)
import Palette
import Route exposing (Route(..))
import Stage exposing (Activity(..))
import Time
import Url exposing (Url)
import View


type alias Flags =
    {}



-- type Exercise
--     = Exercise -- one of a collection (types, list, dict etc -- TBD)


type Mode
    = Running
    | Paused



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , currentStage : Stage.Model
    , mode : Mode
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
                    viewHome model.currentStage

                Settings ->
                    viewSettings model

                Exercises ->
                    viewExercises model

                NotFound ->
                    View.notFound


viewHome : Stage.Model -> List (Element Msg)
viewHome stage =
    let
        timerColor =
            case Stage.activity stage of
                Work ->
                    Palette.color.busy

                Break ->
                    Palette.color.free
    in
    [ column
        [ paddingXY 0 100 ]
        [ View.timer timerColor (Stage.timeRemainingMinSec stage)
        ]
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
            ( { model | currentStage = Stage.update model.currentStage }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , currentStage = Stage.init
      , mode = Running
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Paused ->
            Sub.none

        Running ->
            Time.every 1000 Tick
