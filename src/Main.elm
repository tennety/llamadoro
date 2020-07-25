module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, fill, focused, height, html, layout, padding, paddingXY, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Heroicons.Solid exposing (pause, play, stop)
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
    | ReceivedTick Time.Posix
    | UserClickedPause
    | UserClickedPlay
    | UserClickedReset


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
    layout [ width fill, height fill, centerX ] <|
        column [ centerX ] <|
            case model.route of
                Home ->
                    viewHome model.mode model.currentStage

                Settings ->
                    viewSettings model

                Exercises ->
                    viewExercises model

                NotFound ->
                    View.notFound


viewHome : Mode -> Stage.Model -> List (Element Msg)
viewHome mode stage =
    let
        timerColor =
            case Stage.activity stage of
                Work ->
                    Palette.color.busy

                Break ->
                    Palette.color.free
    in
    [ column
        [ paddingXY 0 50
        , spacing 50
        ]
        [ View.timer timerColor (Stage.timeRemainingMinSec stage)
        , row
            [ centerX, Border.solid, Border.widthXY 0 5, Border.color Palette.color.copy, padding (Palette.scaled 2) ]
            [ playPauseButton (Palette.scaled 6) mode
            , resetButton (Palette.scaled 6)
            ]
        ]
    ]


viewSettings : Model -> List (Element Msg)
viewSettings model =
    [ el [ Region.heading 1 ] (text "Llamadoro - Settings")
    , el [] (text project.description)
    ]


viewExercises : Model -> List (Element Msg)
viewExercises model =
    [ el [ Region.heading 1 ] (text "Llamadoro - Exercises")
    , el [] (text project.description)
    ]



-- app buttons need: size, Msg, icon, description, color, (focus color?)


playPauseButton : Int -> Mode -> Element Msg
playPauseButton size mode =
    let
        ( icon, msg, desc ) =
            case mode of
                Running ->
                    ( pause [], UserClickedPause, "pause" )

                Paused ->
                    ( play [], UserClickedPlay, "play" )
    in
    Input.button
        [ Border.rounded (size // 2)
        , focused [ Border.glow Palette.color.free 1 ]
        , Region.description desc
        ]
        { onPress = Just msg
        , label = el [ height (px size), width (px size), Font.color Palette.color.free ] (html icon)
        }


resetButton : Int -> Element Msg
resetButton size =
    Input.button
        [ Border.rounded (size // 2)
        , focused [ Border.glow Palette.color.busy 1 ]
        , Region.description "stop"
        ]
        { onPress = Just UserClickedReset
        , label = el [ height (px size), width (px size), Font.color Palette.color.busy ] (html (stop []))
        }



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

        ReceivedTick _ ->
            ( { model | currentStage = Stage.update model.currentStage }, Cmd.none )

        UserClickedPlay ->
            ( { model | mode = Running }, Cmd.none )

        UserClickedPause ->
            ( { model | mode = Paused }, Cmd.none )

        UserClickedReset ->
            ( { model | currentStage = Stage.reset model.currentStage, mode = Paused }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , currentStage = Stage.init
      , mode = Paused
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Paused ->
            Sub.none

        Running ->
            Time.every 1000 ReceivedTick
