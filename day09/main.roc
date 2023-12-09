app "advent-2023-roc-day09"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseRow
    |> List.map calculateNextMissingValue
    |> List.sum

expect
    result = part1 inputSample
    result == 114

parseRow : Str -> List I64
parseRow = \line ->
    line
    |> Str.split " "
    |> List.map \str ->
        Str.toI64 str |> okOrCrash "Unable to parse number \(str)"

calculateNextMissingValue = \values ->
    if List.all values \value -> value == 0 then
        0
    else
        lastValue = List.last values |> okOrCrash "Missing last value"
        nextMissingValue = values |> differences |> calculateNextMissingValue
        lastValue + nextMissingValue

expect
    result = calculateNextMissingValue [1, 2, 4, 7]
    result == 11

differences : List (Num a) -> List (Num a)
differences = \values ->
    when values is
        [a, b, ..] -> values |> List.dropFirst 1 |> differences |> List.prepend (b - a)
        _ -> []

expect
    result = differences [1, 2, 4, 7]
    result == [1, 2, 3]

#
# PART 2
#
part2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseRow
    |> List.map calculatePreviousMissingValue
    |> List.sum

expect
    result = part2 inputSample
    result == 2

calculatePreviousMissingValue = \values ->
    if List.all values \value -> value == 0 then
        0
    else
        firstValue = List.first values |> okOrCrash "Missing first value"
        previousMissingValue = values |> differences |> calculatePreviousMissingValue
        firstValue - previousMissingValue

#
# Utilities
#
okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
