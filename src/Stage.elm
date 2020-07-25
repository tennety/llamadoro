module Stage exposing
    ( Activity(..)
    , IntervalLength(..)
    , Model
    , activity
    , init
    , initWithConfig
    , reset
    , timeRemainingMinSec
    , update
    , withLongBreakAfterCount
    , withLongInterval
    , withShortInterval
    , withWorkInterval
    )

import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Quantity


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


type alias Stage =
    { activity : Activity
    , timeRemaining : Duration
    }


type Model
    = Model Config Int Stage


init : Model
init =
    Model initConfig 0 (work initConfig)


initWithConfig : Decode.Value -> Model
initWithConfig configJson =
    let
        config =
            decodeValue decoder configJson
                |> Result.withDefault initConfig
    in
    Model config 0 (work config)


reset : Model -> Model
reset (Model config count stage) =
    Model config count (work config)


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longInterval = Duration.minutes 10
    , shortInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


activity : Model -> Activity
activity (Model config count stage) =
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


update : Model -> Model
update (Model config workDoneCount currentStage) =
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
                    readyFor (longBreak config)

                else
                    readyFor (shortBreak config)

            Break _ ->
                Model config workDoneCount (work config)

    else
        Model config workDoneCount (countDown currentStage)


timedOut : Duration -> Basics.Bool
timedOut time =
    time |> Quantity.equalWithin Quantity.zero (Duration.seconds 0)


countDown : Stage -> Stage
countDown stage =
    { stage | timeRemaining = Quantity.minus Duration.second stage.timeRemaining }


work : Config -> Stage
work config =
    Stage Work config.workInterval


shortBreak : Config -> Stage
shortBreak config =
    Stage (Break Short) config.shortInterval


longBreak : Config -> Stage
longBreak config =
    Stage (Break Long) config.longInterval


timeRemainingMinSec : Model -> ( Int, Int )
timeRemainingMinSec (Model config count stage) =
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
