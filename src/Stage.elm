module Stage exposing
    ( Activity(..)
    , Model
    , activity
    , init
    , timeRemainingMinSec
    , update
    , withLongBreakAfterCount
    , withLongBreakInterval
    , withShortBreakInterval
    , withWorkInterval
    )

import Duration exposing (Duration)
import Quantity


type Activity
    = Work
    | Break


type alias Config =
    { workInterval : Duration
    , shortBreakInterval : Duration
    , longBreakInterval : Duration
    , longBreakAfterCount : Int
    }


type alias Stage =
    { timeRemaining : Duration
    , activity : Activity
    }


type Model
    = Model Config Int Stage


init : Model
init =
    Model initConfig 0 (work initConfig)


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longBreakInterval = Duration.minutes 10
    , shortBreakInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


activity : Model -> Activity
activity (Model config count stage) =
    stage.activity


withWorkInterval : Duration -> Model -> Model
withWorkInterval duration (Model config count stage) =
    Model { config | workInterval = duration } count stage


withShortBreakInterval : Duration -> Model -> Model
withShortBreakInterval duration (Model config count stage) =
    Model { config | shortBreakInterval = duration } count stage


withLongBreakInterval : Duration -> Model -> Model
withLongBreakInterval duration (Model config count stage) =
    Model { config | longBreakInterval = duration } count stage


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

            Break ->
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
    Stage config.workInterval Work


shortBreak : Config -> Stage
shortBreak config =
    Stage config.shortBreakInterval Break


longBreak : Config -> Stage
longBreak config =
    Stage config.longBreakInterval Break


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
