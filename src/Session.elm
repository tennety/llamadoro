module Session exposing
    ( Model
    , initWithConfig
    , onBreak
    , reset
    , setStartTime
    , update
    , view
    , workSessionCount
    , working
    )

import Basics
import Browser.Dom exposing (Element)
import Duration exposing (Duration)
import Element exposing (Element)
import Json.Decode as Decode exposing (Decoder, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Palette
import Quantity
import Session.Actions exposing (Action(..), Activity(..), IntervalLength(..))
import Session.Timer as Timer exposing (Timer)
import Time


type alias Config =
    { workInterval : Duration
    , shortInterval : Duration
    , longInterval : Duration
    , longBreakAfterCount : Int
    }


type Model
    = Model
        { config : Config
        , workSessionsDone : Basics.Int
        , activity : Activity
        , timer : Timer
        }


initWithConfig : Decode.Value -> Model
initWithConfig configJson =
    let
        config =
            decodeValue decoder configJson
                |> Result.withDefault initConfig
    in
    Model
        { config = config
        , workSessionsDone = 0
        , activity = Work
        , timer = Timer.fromDurationAndTimestamp config.workInterval (Time.millisToPosix 0)
        }


reset : Model -> Model
reset (Model model) =
    Model { model | timer = model.timer |> Timer.withDuration model.config.workInterval }


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longInterval = Duration.minutes 10
    , shortInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


onBreak : Model -> Bool
onBreak =
    Basics.not << working


working : Model -> Bool
working (Model model) =
    case model.activity of
        Work ->
            Basics.True

        Break _ ->
            Basics.False


workSessionCount : Model -> Basics.Int
workSessionCount (Model model) =
    model.workSessionsDone


setStartTime : Time.Posix -> Model -> Model
setStartTime newTime (Model model) =
    Model { model | timer = model.timer |> Timer.withTimeStamp newTime }


update : Time.Posix -> Model -> ( Model, Action )
update newTime (Model model) =
    if Timer.timedOut model.timer then
        case model.activity of
            Work ->
                let
                    workDoneCount =
                        model.workSessionsDone + 1

                    nextModel =
                        Model { model | workSessionsDone = workDoneCount }

                    needsLongBreak =
                        Basics.modBy model.config.longBreakAfterCount workDoneCount == 0
                in
                if needsLongBreak then
                    ( longBreak newTime nextModel, SwitchedActivity )

                else
                    ( shortBreak newTime nextModel, SwitchedActivity )

            Break _ ->
                ( work newTime (Model model), SwitchedActivity )

    else
        ( Model { model | timer = Timer.countDown newTime model.timer }, CountedDown )


work : Time.Posix -> Model -> Model
work timeStamp (Model model) =
    Model
        { model
            | activity = Work
            , timer = Timer.fromDurationAndTimestamp model.config.workInterval timeStamp
        }


shortBreak : Time.Posix -> Model -> Model
shortBreak timeStamp (Model model) =
    Model
        { model
            | activity = Break Short
            , timer = Timer.fromDurationAndTimestamp model.config.shortInterval timeStamp
        }


longBreak : Time.Posix -> Model -> Model
longBreak timeStamp (Model model) =
    Model
        { model
            | activity = Break Long
            , timer = Timer.fromDurationAndTimestamp model.config.longInterval timeStamp
        }


decoder : Decoder Config
decoder =
    Decode.succeed Config
        |> required "workInterval" durationDecoder
        |> required "shortInterval" durationDecoder
        |> required "longInterval" durationDecoder
        |> required "longBreakAfterCount" int


durationDecoder : Decoder Duration
durationDecoder =
    int
        |> Decode.map Basics.toFloat
        |> Decode.map Duration.seconds



-- VIEW --


view : Model -> Element msg
view (Model model) =
    let
        timerColor =
            if working (Model model) then
                Palette.color.busy

            else
                Palette.color.free
    in
    Timer.view timerColor model.timer
