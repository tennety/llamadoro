module Session exposing
    ( Activity(..)
    , IntervalLength(..)
    , Model
    , activity
    , initWithConfig
    , reset
    , setStartTime
    , timeRemainingMinSec
    , update
    , withLongBreakAfterCount
    , withLongInterval
    , withShortInterval
    , withWorkInterval
    )

import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Quantity
import Time


type Activity
    = Work
    | Break IntervalLength


type IntervalLength
    = Short
    | Long


type alias Config =
    { workInterval : Duration
    , shortInterval : Duration
    , longInterval : Duration
    , longBreakAfterCount : Int
    }


type alias Session =
    { activity : Activity
    , timeRemaining : Duration
    , timeStamp : Time.Posix
    }


type Model
    = Model Config Int Session


initWithConfig : Decode.Value -> Model
initWithConfig configJson =
    let
        config =
            decodeValue decoder configJson
                |> Result.withDefault initConfig
    in
    Model config 0 (work (Time.millisToPosix 0) config)


reset : Model -> Model
reset (Model config count currentStage) =
    Model config count (work currentStage.timeStamp config)


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longInterval = Duration.minutes 10
    , shortInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


activity : Model -> Activity
activity (Model _ _ stage) =
    stage.activity


withWorkInterval : Duration -> Model -> Model
withWorkInterval duration (Model config count stage) =
    Model { config | workInterval = duration } count stage


withShortInterval : Duration -> Model -> Model
withShortInterval duration (Model config count stage) =
    Model { config | shortInterval = duration } count stage


withLongInterval : Duration -> Model -> Model
withLongInterval duration (Model config count stage) =
    Model { config | longInterval = duration } count stage


withLongBreakAfterCount : Int -> Model -> Model
withLongBreakAfterCount count (Model config count_ stage) =
    Model { config | longBreakAfterCount = count } count_ stage


setStartTime : Time.Posix -> Model -> Model
setStartTime newTime (Model config workDoneCount currentStage) =
    Model config workDoneCount { currentStage | timeStamp = newTime }


update : Time.Posix -> Model -> Model
update newTime (Model config workDoneCount currentStage) =
    if timedOut currentStage.timeRemaining then
        case currentStage.activity of
            Work ->
                let
                    readyFor =
                        Model config (workDoneCount + 1)

                    needsLongBreak =
                        Basics.modBy config.longBreakAfterCount (workDoneCount + 1) == 0
                in
                if needsLongBreak then
                    readyFor (longBreak newTime config)

                else
                    readyFor (shortBreak newTime config)

            Break _ ->
                Model config workDoneCount (work newTime config)

    else
        Model config workDoneCount (countDown newTime currentStage)


timedOut : Duration -> Basics.Bool
timedOut time =
    time |> Quantity.equalWithin Quantity.zero (Duration.seconds 0)


countDown : Time.Posix -> Session -> Session
countDown newTime stage =
    let
        elapsed =
            Duration.from stage.timeStamp newTime
    in
    { stage | timeStamp = newTime, timeRemaining = Quantity.minus elapsed stage.timeRemaining }


work : Time.Posix -> Config -> Session
work timeStamp config =
    Session Work config.workInterval timeStamp


shortBreak : Time.Posix -> Config -> Session
shortBreak timeStamp config =
    Session (Break Short) config.shortInterval timeStamp


longBreak : Time.Posix -> Config -> Session
longBreak timeStamp config =
    Session (Break Long) config.longInterval timeStamp


timeRemainingMinSec : Model -> ( Int, Int )
timeRemainingMinSec (Model _ _ stage) =
    let
        seconds =
            stage.timeRemaining
    in
    ( seconds |> Duration.inMinutes
    , seconds |> Quantity.fractionalModBy Duration.minute |> Duration.inSeconds
    )
        |> Tuple.mapBoth Basics.floor Basics.floor


decoder : Decoder Config
decoder =
    Decode.succeed Config
        |> required "workInterval" durationDecoder
        |> required "shortInterval" durationDecoder
        |> required "longInterval" durationDecoder
        |> required "longBreakAfterCount" int



-- initConfig.longBreakAfterCount


durationDecoder : Decoder Duration
durationDecoder =
    int
        |> Decode.map Basics.toFloat
        |> Decode.map Duration.seconds
