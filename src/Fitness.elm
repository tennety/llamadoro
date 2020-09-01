module Fitness exposing (Exercise, ExercisesByLevel, Level(..), buildExercises, decodeExerciseInfo, decodeLevel, getExercisesForLevel, levelFieldDecoder)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, decodeValue, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)


type Level
    = Beginner
    | Intermediate
    | Advanced


type alias ExercisesByLevel =
    { beginner : Array Exercise
    , intermediate : Array Exercise
    , advanced : Array Exercise
    }


type alias Exercise =
    { name : String
    , reps : Int
    , directions : List String
    }



-- Temporary types --


type alias ExerciseInfo =
    { name : String
    , variations : List Variation
    }


type alias Variation =
    { reps : Int
    , level : Level
    , directions : List String
    }


decodeLevel : Decode.Value -> Level
decodeLevel config =
    config
        |> decodeValue (Decode.field "fitnessLevel" levelFieldDecoder)
        |> Result.withDefault Beginner


decodeExerciseInfo : Decode.Value -> List ExerciseInfo
decodeExerciseInfo exerciseJson =
    exerciseJson
        |> decodeValue (list exerciseInfoDecoder)
        |> Result.withDefault []


buildExercises : List ExerciseInfo -> ExercisesByLevel
buildExercises exercises =
    let
        byLevel =
            ExercisesByLevel Array.empty Array.empty Array.empty
    in
    List.foldl builder byLevel exercises


getExercisesForLevel : Level -> ExercisesByLevel -> Array Exercise
getExercisesForLevel level byLevel =
    case level of
        Beginner ->
            byLevel.beginner

        Intermediate ->
            byLevel.intermediate

        Advanced ->
            byLevel.advanced


builder : ExerciseInfo -> ExercisesByLevel -> ExercisesByLevel
builder { name, variations } byLevel =
    let
        func var rec =
            case var.level of
                Beginner ->
                    { rec | beginner = Array.push (buildFromVariation name var) rec.beginner }

                Intermediate ->
                    { rec | intermediate = Array.push (buildFromVariation name var) rec.intermediate }

                Advanced ->
                    { rec | advanced = Array.push (buildFromVariation name var) rec.advanced }
    in
    List.foldl func byLevel variations


buildFromVariation : String -> Variation -> Exercise
buildFromVariation name variation =
    Exercise name variation.reps variation.directions


levelFieldDecoder : Decoder Level
levelFieldDecoder =
    string
        |> Decode.andThen fitnessLevelDecoder


variationDecoder : Decoder Variation
variationDecoder =
    Decode.succeed Variation
        |> required "reps" int
        |> required "level" levelFieldDecoder
        |> optional "directions" (list string) []


exerciseInfoDecoder : Decoder ExerciseInfo
exerciseInfoDecoder =
    Decode.succeed ExerciseInfo
        |> required "name" string
        |> required "variations" (list variationDecoder)


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
