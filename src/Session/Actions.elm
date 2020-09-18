module Session.Actions exposing (Action(..), Activity(..), IntervalLength(..))


type Activity
    = Work
    | Break IntervalLength


type IntervalLength
    = Short
    | Long


type Action
    = CountedDown
    | SwitchedActivity
