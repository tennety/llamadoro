module Session exposing
    ( Activity(..)
    , Model
    , initWithConfig
    , reset
    , setStartTime
    , update
    , view
    , workSessionCount
    , working
    )

import Basics
import Duration exposing (Duration)
import Element exposing (Element)
import Json.Decode as Decode exposing (Decoder, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Palette
import Quantity
import Session.Timer as Timer exposing (Timer)
import Time


type alias Config =
    { workInterval : Duration
    , shortInterval : Duration
    , longInterval : Duration
    , longBreakAfterCount : Int
    }


type Activity
    = Work
    | Break IntervalLength


type IntervalLength
    = Short
    | Long


type alias Handler msg =
    { onCountDown : msg
    , onSwitchedActivity : Activity -> msg
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
    Model
        { model
            | timer = model.timer |> Timer.withDuration model.config.workInterval
            , activity = Work
        }


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longInterval = Duration.minutes 10
    , shortInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


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


update : Handler msg -> Time.Posix -> Model -> ( Model, msg )
update handler newTime (Model model) =
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
                    setActivity (Break Long) handler.onSwitchedActivity newTime nextModel

                else
                    setActivity (Break Short) handler.onSwitchedActivity newTime nextModel

            Break _ ->
                setActivity Work handler.onSwitchedActivity newTime (Model model)

    else
        ( Model { model | timer = Timer.countDown newTime model.timer }, handler.onCountDown )


configInterval : Activity -> Config -> Duration
configInterval activity config =
    case activity of
        Work ->
            config.workInterval

        Break interval ->
            case interval of
                Long ->
                    config.longInterval

                Short ->
                    config.shortInterval


setActivity : Activity -> (Activity -> msg) -> Time.Posix -> Model -> ( Model, msg )
setActivity activity announceSwitch timeStamp (Model model) =
    ( Model
        { model
            | activity = activity
            , timer = Timer.fromDurationAndTimestamp (configInterval activity model.config) timeStamp
        }
    , announceSwitch activity
    )


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
