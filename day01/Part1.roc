interface Part1
    exposes [process]
    imports ["input-sample-1.txt" as inputSample : Str]

process = \input ->
    input
    |> Str.split "\n"
    |> List.map buildNumber
    |> List.sum

buildNumber = \str ->
    str |> numbers |> joinFirstAndLast |> Str.toI32 |> Result.withDefault 0

numbers = \input ->
    input
    |> Str.graphemes
    |> List.keepIf isNumber

isNumber = \grapheme ->
    List.range { start: At 0, end: At 9 }
    |> List.map Num.toStr
    |> List.contains grapheme

joinFirstAndLast = \list ->
    first = list |> List.first |> Result.withDefault ""
    last = list |> List.last |> Result.withDefault ""
    Str.concat first last

expect
    result = process inputSample
    result == 142
