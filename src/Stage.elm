module Stage exposing
    ( Activity(..)
    , Config
    , Model
    , inMinutesAndSeconds
    , initConfig
    , toModel
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


type alias Model =
    { config : Config
    , workDoneCount : Int
    , currentStage : Stage
    }


initConfig : Config
initConfig =
    { workInterval = Duration.minutes 25
    , longBreakInterval = Duration.minutes 10
    , shortBreakInterval = Duration.minutes 5
    , longBreakAfterCount = 4
    }


withWorkInterval : Duration -> Config -> Config
withWorkInterval duration config =
    { config | workInterval = duration }


withShortBreakInterval : Duration -> Config -> Config
withShortBreakInterval duration config =
    { config | shortBreakInterval = duration }


withLongBreakInterval : Duration -> Config -> Config
withLongBreakInterval duration config =
    { config | longBreakInterval = duration }


withLongBreakAfterCount : Int -> Config -> Config
withLongBreakAfterCount count config =
    { config | longBreakAfterCount = count }


toModel : Config -> Model
toModel config =
    Model config 0 (work config)


update : Model -> Model
update ({ config, currentStage } as model) =
    if timedOut currentStage.timeRemaining then
        case currentStage.activity of
            Work ->
                let
                    newModel =
                        { model | workDoneCount = model.workDoneCount + 1 }
                in
                if Basics.modBy config.longBreakAfterCount newModel.workDoneCount == 0 then
                    { newModel | currentStage = longBreak config }

                else
                    { newModel | currentStage = shortBreak config }

            Break ->
                { model | currentStage = work config }

    else
        { model | currentStage = countDown model.currentStage }


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


inMinutesAndSeconds : Duration -> String
inMinutesAndSeconds seconds =
    [ seconds |> Duration.inMinutes
    , seconds |> Quantity.fractionalModBy Duration.minute |> Duration.inSeconds
    ]
        |> List.map (Basics.floor >> String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"
