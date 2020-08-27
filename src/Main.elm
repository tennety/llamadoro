module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, fill, focused, height, html, layout, moveDown, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Heroicons.Solid exposing (pause, play, stop)
import Html exposing (Html)
import Json.Decode as Decode
import Palette
import Route exposing (Route(..))
import Session exposing (Activity(..))
import Task
import Time
import Url exposing (Url)
import View


type alias Flags =
    { config : Decode.Value
    , exercises : Decode.Value
    }


type Mode
    = Running
    | Paused



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , currentSession : Session.Model
    , mode : Mode
    }


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | ReceivedCurrentTime Time.Posix
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
    layout [] <|
        column [ height fill, width fill ] <|
            case model.route of
                Home ->
                    viewHome model.mode model.currentSession

                Settings ->
                    viewSettings model

                Exercises ->
                    viewExercises model

                NotFound ->
                    View.notFound


viewHome : Mode -> Session.Model -> List (Element Msg)
viewHome mode session =
    let
        timerColor =
            if Session.working session then
                Palette.color.busy

            else
                Palette.color.free

        doneSessions =
            Session.workSessionCount session
    in
    [ column
        [ paddingXY 0 50
        , spacing 40
        , height fill
        , width fill
        ]
        [ View.timer timerColor (Session.timeRemainingMinSec session)
        , row
            [ centerX
            , width fill
            ]
            [ playPauseButton (Palette.scaled 6) mode
            , resetButton (Palette.scaled 6)
            ]
        , el
            [ width fill
            , Border.solid
            , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color Palette.color.copy
            , moveDown (Basics.toFloat <| Palette.scaled 2)
            ]
            (View.workCountLlama doneSessions)
        , column
            [ width fill
            , height fill
            , paddingXY 10 0
            ]
            [ paragraph
                [ Region.heading 2
                , Font.center
                , Font.size (Palette.scaled 3)
                , Font.family Palette.fontFamily.title
                , Font.color Palette.color.copy
                ]
                (if Session.working session then
                    [ text "You're llama-dorable!" ]

                 else
                    [ text "How about some quick fitness?" ]
                )
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
        , centerX
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
        , centerX
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

        ReceivedCurrentTime timeStamp ->
            ( { model | currentSession = Session.setStartTime timeStamp model.currentSession }, Cmd.none )

        ReceivedTick timeStamp ->
            ( { model | currentSession = Session.update timeStamp model.currentSession }, Cmd.none )

        UserClickedPlay ->
            ( { model | mode = Running }
            , Task.perform ReceivedCurrentTime Time.now
            )

        UserClickedPause ->
            ( { model | mode = Paused }, Cmd.none )

        UserClickedReset ->
            ( { model | currentSession = Session.reset model.currentSession, mode = Paused }, Cmd.none )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , currentSession = Session.initWithConfig flags.config
      , mode = Paused
      }
    , Task.perform ReceivedCurrentTime Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Paused ->
            Sub.none

        Running ->
            Time.every 500 ReceivedTick
