app "advent-2023-roc-day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample-1.txt" as inputSample1 : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 1: \(Num.toStr (part2 inputFull))"

part1 = \input ->
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

part2 = \_ ->
    0

expect
    result = part1 inputSample1
    result == 142

# expect
#     result = part2 inputSample2
#     result == 281
