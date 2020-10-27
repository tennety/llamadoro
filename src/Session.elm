module Session exposing
    ( Model
    , initWithConfig
    , onBreak
    , reset
    , setStartTime
    , update
    , view
    , withLongBreakAfterCount
    , withLongInterval
    , withShortInterval
    , withWorkInterval
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


type Session
    = Session Activity Timer


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
    let
        session =
            Session Work (timer |> Timer.withDuration config.workInterval)
    in
    Model config count session


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
    Model config workDoneCount (Session activity (timer |> Timer.withTimeStamp newTime))


update : Time.Posix -> Model -> ( Model, Action )
update newTime (Model config workDoneCount (Session activity timer)) =
    if Timer.timedOut timer then
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
        ( Model config workDoneCount (Session activity (Timer.countDown newTime timer)), CountedDown )


work : Time.Posix -> Config -> Session
work timeStamp config =
    Session Work (Timer.fromDurationAndTimestamp config.workInterval timeStamp)


shortBreak : Time.Posix -> Config -> Session
shortBreak timeStamp config =
    Session (Break Short) (Timer.fromDurationAndTimestamp config.shortInterval timeStamp)


longBreak : Time.Posix -> Config -> Session
longBreak timeStamp config =
    Session (Break Long) (Timer.fromDurationAndTimestamp config.longInterval timeStamp)


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
view ((Model _ _ (Session activity timer)) as model) =
    let
        timerColor =
            if working model then
                Palette.color.busy

            else
                Palette.color.free
    in
    Timer.view timerColor timer
