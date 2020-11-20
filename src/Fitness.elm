module Fitness exposing (Exercise, ExercisesByLevel, Level, buildExercises, decodeExerciseInfo, decodeLevel, defaultExercise, getExercisesForLevel, levelFieldDecoder, viewExercise)

import Array exposing (Array)
import Element exposing (Element, column, fill, height, padding, paddingXY, paragraph, spacing, text, textColumn, width)
import Element.Font as Font
import Element.Region as Region
import Json.Decode as Decode exposing (Decoder, decodeValue, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Palette


type Level
    = Beginner
    | Intermediate
    | Advanced


type ExercisesByLevel
    = ExercisesByLevel
        { beginner : Array Exercise
        , intermediate : Array Exercise
        , advanced : Array Exercise
        }


type Exercise
    = Exercise
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


defaultExercise : Exercise
defaultExercise =
    Exercise
        { name = "Movement"
        , reps = 4
        , directions =
            [ "Stand up and stretch."
            , "Walk around the office or up and down the stairs."
            ]
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
            ExercisesByLevel { beginner = Array.empty, intermediate = Array.empty, advanced = Array.empty }
    in
    List.foldl builder byLevel exercises


getExercisesForLevel : Level -> ExercisesByLevel -> Array Exercise
getExercisesForLevel level (ExercisesByLevel byLevel) =
    case level of
        Beginner ->
            byLevel.beginner

        Intermediate ->
            byLevel.intermediate

        Advanced ->
            byLevel.advanced


builder : ExerciseInfo -> ExercisesByLevel -> ExercisesByLevel
builder { name, variations } (ExercisesByLevel byLevel) =
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
    ExercisesByLevel (List.foldl func byLevel variations)


buildFromVariation : String -> Variation -> Exercise
buildFromVariation name variation =
    Exercise { name = name, reps = variation.reps, directions = variation.directions }


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


viewDirection : String -> Element msg
viewDirection direction =
    paragraph
        [ Font.color Palette.color.copy
        , Font.size (Palette.scaled 2)
        ]
        [ text direction ]


viewExercise : Exercise -> Element msg
viewExercise (Exercise exercise) =
    textColumn
        [ width fill
        , height fill
        , padding (Palette.scaled 2)
        , Font.family Palette.fontFamily.title
        ]
        [ paragraph
            [ Region.heading 3
            , Font.size (Palette.scaled 2)
            , Font.family Palette.fontFamily.title
            , Font.color Palette.color.blue
            ]
            [ text exercise.name
            ]
        , paragraph
            [ Font.family Palette.fontFamily.title
            , Font.color (Palette.color.copy |> Palette.withOpacity 0.7)
            ]
            [ text <| String.fromInt exercise.reps ++ " repetitions"
            ]
        , column
            [ Font.color Palette.color.copy
            , paddingXY 0 (Palette.scaled 2)
            , spacing (Palette.scaled 2)
            ]
            (List.map
                viewDirection
                exercise.directions
            )
        ]
