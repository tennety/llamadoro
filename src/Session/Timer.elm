module Session.Timer exposing (Timer, countDown, fromDurationAndTimestamp, timedOut, view, withDuration, withTimeStamp)

import Duration exposing (Duration)
import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, height, paddingXY, row, shrink, text, width)
import Element.Font as Font
import Element.Region as Region
import Palette
import Quantity
import Time


type Timer
    = Timer
        { timeRemaining : Duration
        , timeStamp : Time.Posix
        }


fromDurationAndTimestamp : Duration -> Time.Posix -> Timer
fromDurationAndTimestamp timeRemaining timeStamp =
    Timer { timeRemaining = timeRemaining, timeStamp = timeStamp }


withDuration : Duration -> Timer -> Timer
withDuration time (Timer timer) =
    Timer { timeRemaining = time, timeStamp = timer.timeStamp }


withTimeStamp : Time.Posix -> Timer -> Timer
withTimeStamp posix (Timer timer) =
    Timer { timeRemaining = timer.timeRemaining, timeStamp = posix }


timedOut : Timer -> Basics.Bool
timedOut (Timer timer) =
    timer.timeRemaining |> Quantity.equalWithin (Duration.seconds 0.1) Quantity.zero


countDown : Time.Posix -> Timer -> Timer
countDown newTime (Timer timer) =
    let
        elapsed =
            Duration.from timer.timeStamp newTime
    in
    Timer { timer | timeStamp = newTime, timeRemaining = Quantity.max (Quantity.minus elapsed timer.timeRemaining) (Duration.seconds 0) }


timeRemainingMinSec : Timer -> ( Int, Int )
timeRemainingMinSec (Timer timer) =
    let
        seconds =
            timer.timeRemaining
    in
    ( seconds |> Duration.inMinutes
    , seconds |> Quantity.fractionalModBy Duration.minute |> Duration.inSeconds
    )
        |> Tuple.mapBoth Basics.floor Basics.floor


view : Element.Color -> Timer -> Element msg
view activityColor timer =
    let
        ( mins, secs ) =
            timeRemainingMinSec timer
    in
    row
        [ Font.family Palette.fontFamily.display
        , Font.size (Palette.scaled 8)
        , Font.medium
        , Font.color activityColor
        , centerX
        , width fill
        , Region.description <| String.fromInt mins ++ " minutes remaining."
        ]
        [ el
            [ width (fillPortion 1)
            , Font.alignRight
            ]
            (mins |> String.fromInt |> String.padLeft 2 '0' |> text)
        , el
            [ width shrink
            , paddingXY 5 0
            , Font.size (Palette.scaled 7)
            , Font.center
            , alignTop
            ]
            (text ":")
        , el
            [ width (fillPortion 1)
            , Font.alignLeft
            ]
            (secs |> String.fromInt |> String.padLeft 2 '0' |> text)
        ]
