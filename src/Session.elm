module Session exposing
    ( Model
    , initWithConfig
    , onBreak
    , reset
    , setStartTime
    , timeRemainingMinSec
    , update
    , withLongBreakAfterCount
    , withLongInterval
    , withShortInterval
    , withWorkInterval
    , workSessionCount
    , working
    )

import Basics
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder, decodeValue, int)
import Json.Decode.Pipeline exposing (required)
import Quantity
import Session.Actions exposing (Action(..), Activity(..), IntervalLength(..))
import Time


type alias Config =
    { workInterval : Duration
    , shortInterval : Duration
    , longInterval : Duration
    , longBreakAfterCount : Int
    }


type Session
    = Session Activity Timer


type alias Timer =
    { timeRemaining : Duration
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
reset (Model config count (Session _ timer)) =
    Model config count (work timer.timeStamp config)


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
working (Model _ _ (Session activity _)) =
    case activity of
        Work ->
            Basics.True

        Break _ ->
            Basics.False


workSessionCount : Model -> Basics.Int
workSessionCount (Model _ count _) =
    count


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
setStartTime newTime (Model config workDoneCount (Session activity timer)) =
    Model config workDoneCount (Session activity { timer | timeStamp = newTime })


update : Time.Posix -> Model -> ( Model, Action )
update newTime (Model config workDoneCount (Session activity timer)) =
    if timedOut timer.timeRemaining then
        case activity of
            Work ->
                let
                    readyFor =
                        Model config (workDoneCount + 1)

                    needsLongBreak =
                        Basics.modBy config.longBreakAfterCount (workDoneCount + 1) == 0
                in
                if needsLongBreak then
                    ( readyFor (longBreak newTime config), SwitchedActivity )

                else
                    ( readyFor (shortBreak newTime config), SwitchedActivity )

            Break _ ->
                ( Model config workDoneCount (work newTime config), SwitchedActivity )

    else
        ( Model config workDoneCount (Session activity (countDown newTime timer)), CountedDown )


timedOut : Duration -> Basics.Bool
timedOut time =
    time |> Quantity.equalWithin (Duration.seconds 0.1) Quantity.zero


countDown : Time.Posix -> Timer -> Timer
countDown newTime timer =
    let
        elapsed =
            Duration.from timer.timeStamp newTime
    in
    { timer | timeStamp = newTime, timeRemaining = Quantity.max (Quantity.minus elapsed timer.timeRemaining) (Duration.seconds 0) }


work : Time.Posix -> Config -> Session
work timeStamp config =
    Session Work (Timer config.workInterval timeStamp)


shortBreak : Time.Posix -> Config -> Session
shortBreak timeStamp config =
    Session (Break Short) (Timer config.shortInterval timeStamp)


longBreak : Time.Posix -> Config -> Session
longBreak timeStamp config =
    Session (Break Long) (Timer config.longInterval timeStamp)


timeRemainingMinSec : Model -> ( Int, Int )
timeRemainingMinSec (Model _ _ (Session _ timer)) =
    let
        seconds =
            timer.timeRemaining
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
