module Fitness exposing (Exercise, Level(..), decodeLevel, decoder, levelFieldDecoder)

import Json.Decode as Decode exposing (Decoder, decodeValue, int, string)
import Json.Decode.Pipeline exposing (optional, required)


type Level
    = Beginner
    | Intermediate
    | Advanced


type alias Exercise =
    { name : String
    , reps : Int
    , level : Level
    , directions : String
    }


decodeLevel : Decode.Value -> Level
decodeLevel config =
    config
        |> decodeValue (Decode.field "fitnessLevel" levelFieldDecoder)
        |> Result.withDefault Beginner


decoder : Decoder Exercise
decoder =
    Decode.succeed Exercise
        |> required "name" string
        |> required "reps" int
        |> optional "level" levelFieldDecoder Beginner
        |> optional "directions" string ""


levelFieldDecoder : Decoder Level
levelFieldDecoder =
    string
        |> Decode.andThen fitnessLevelDecoder


fitnessLevelDecoder : String -> Decoder Level
fitnessLevelDecoder fitnessLevel =
    case fitnessLevel of
        "beginner" ->
            Decode.succeed Beginner

        "intermediate" ->
            Decode.succeed Intermediate

        "advanced" ->
            Decode.succeed Advanced

        _ ->
            Decode.fail "Unknown exercise level"
