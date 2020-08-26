module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Color
import Element exposing (Element, alignTop, centerX, column, el, fill, focused, height, html, layout, maximum, minimum, moveDown, moveUp, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Gen.Graphics as Graphics
import Heroicons.Solid exposing (pause, play, stop)
import Html exposing (Html)
import Json.Decode as Decode
import Palette
import Route exposing (Route(..))
import Session exposing (Activity(..))
import Svg.Attributes as SA
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
            case Session.activity session of
                Work ->
                    Palette.color.busy

                Break _ ->
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
            (workLlama doneSessions)
        , column
            [ width fill
            , height fill
            , centerX
            ]
            [ Debug.log "exercise" Element.none ]
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


workLlama : Int -> Element Msg
workLlama count =
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
