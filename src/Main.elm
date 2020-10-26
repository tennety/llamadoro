module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, centerX, column, el, fill, focused, height, html, layout, moveDown, padding, paddingXY, paragraph, px, row, spaceEvenly, spacing, spacingXY, text, textColumn, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Fitness exposing (viewExercise)
import Heroicons.Solid exposing (pause, play, stop)
import Html exposing (Html)
import Json.Decode as Decode
import Palette
import Random
import Random.Array
import Route exposing (Route(..))
import Session
import Session.Actions as SessionActions exposing (Action(..))
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
    , mode : Mode
    , currentSession : Session.Model
    , fitnessLevel : Fitness.Level
    , exercises : Fitness.ExercisesByLevel
    , nextExercise : Fitness.Exercise
    }


type Msg
    = BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | ReceivedCurrentTime Time.Posix
    | ReceivedTick Time.Posix
    | ReceivedSampleExercise (Maybe Fitness.Exercise)
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
                    viewHome model.mode model.nextExercise model.currentSession

                Settings ->
                    viewSettings model

                Exercises ->
                    viewExercises model

                NotFound ->
                    View.notFound


viewHome : Mode -> Fitness.Exercise -> Session.Model -> List (Element Msg)
viewHome mode exercise session =
    let
        ( timerColor, caption, content ) =
            if Session.working session then
                ( Palette.color.busy
                , "You're llama-dorable!"
                , Element.none
                )

            else
                ( Palette.color.free
                , "How about some quick fitness?"
                , viewExercise exercise
                )

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
                [ text caption ]
            , content
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


fetchNextExercise : Fitness.Level -> Fitness.ExercisesByLevel -> Cmd Msg
fetchNextExercise level exercises =
    let
        levelExercises =
            Fitness.getExercisesForLevel level exercises
    in
    Random.generate ReceivedSampleExercise (Random.Array.sample levelExercises)



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
            let
                ( newSession, action ) =
                    Session.update timeStamp model.currentSession

                cmd =
                    case action of
                        SessionActions.CountedDown ->
                            Cmd.none

                        SessionActions.SwitchedActivity ->
                            if Session.onBreak newSession then
                                fetchNextExercise model.fitnessLevel model.exercises

                            else
                                Cmd.none
            in
            ( { model | currentSession = newSession }, cmd )

        ReceivedSampleExercise maybeExercise ->
            let
                exercise =
                    case maybeExercise of
                        Just newExercise ->
                            newExercise

                        Nothing ->
                            model.nextExercise
            in
            ( { model | nextExercise = exercise }, Cmd.none )

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

        model =
            { key = key
            , route = route
            , mode = Paused
            , currentSession = Session.initWithConfig flags.config
            , fitnessLevel = Fitness.decodeLevel flags.config
            , exercises = flags.exercises |> Fitness.decodeExerciseInfo |> Fitness.buildExercises
            , nextExercise = Fitness.defaultExercise
            }
    in
    ( model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Paused ->
            Sub.none

        Running ->
            Time.every 500 ReceivedTick
