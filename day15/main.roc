app "advent-2023-roc-day15"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        parser.Core.{
            const,
            keep,
            skip,
            oneOf,
            chompWhile,
            sepBy,
        },
        parser.String.{ parseStr, string, digits },
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Step : { label : List U8, operation : [RemoveLense, AddLense Nat] }
Lense : { label : List U8, focalLength : Nat }

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input |> parseCharGroups |> List.map hashChars |> List.sum

expect
    result = part1 inputExample1
    result == 1320

parseCharGroups : Str -> List (List U8)
parseCharGroups = \input ->
    input |> Str.trim |> Str.split "," |> List.map Str.toUtf8

hashChars : List U8 -> Nat
hashChars = \chars ->
    chars
    |> List.walk 0 \total, char ->
        (total + (Num.toNat char)) * 17 % 256

#
# PART 2
#
part2 : Str -> Nat
part2 = \input ->
    input
    |> parseSteps
    |> List.walk (Dict.empty {}) performStep
    |> Dict.toList
    |> List.map focusingPowerForBox
    |> List.sum

expect
    result = part2 inputExample1
    result == 145

performStep : Dict Nat (List Lense), Step -> Dict Nat (List Lense)
performStep = \boxes, step ->
    boxId = hashChars step.label
    lenses = boxes |> Dict.get boxId |> Result.withDefault []
    boxes |> Dict.insert boxId (updateLenses lenses step)

updateLenses : List Lense, Step -> List Lense
updateLenses = \lenses, step ->
    when step.operation is
        RemoveLense -> List.dropIf lenses \lense -> lense.label == step.label
        AddLense focalLength ->
            when List.findFirstIndex lenses \lense -> lense.label == step.label is
                Ok index -> lenses |> List.set index { label: step.label, focalLength }
                Err NotFound -> lenses |> List.append { label: step.label, focalLength }

focusingPowerForBox : (Nat, List Lense) -> Nat
focusingPowerForBox = \(boxId, lenses) ->
    lenses
    |> List.mapWithIndex \lense, index -> (boxId + 1) * lense.focalLength * (index + 1)
    |> List.sum

#
# Parser
#
parseSteps : Str -> List Step
parseSteps = \input ->
    when parseStr stepsParser (Str.trim input) is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

stepsParser =
    const (\steps -> steps)
    |> keep (stepParser |> sepBy (string ","))

stepParser =
    const (\label -> \operation -> { label, operation })
    |> keep (chompWhile \char -> char >= 'a' && char <= 'z')
    |> keep operationParser

expect
    result = parseStr stepParser "ab=1"
    result == Ok { label: ['a', 'b'], operation: AddLense 1 }

operationParser =
    oneOf [
        const RemoveLense |> skip (string "-"),
        const (\focalLength -> AddLense focalLength) |> skip (string "=") |> keep digits,
    ]

expect
    result = parseStr operationParser "-"
    result == Ok RemoveLense

expect
    result = parseStr operationParser "=1"
    result == Ok (AddLense 1)

#
# Utilities
#

# debug = \value ->
#     dbg value

#     value

# indexes : List a -> List Nat
# indexes = \list ->
#     List.range { start: At 0, end: Length (Num.toNat (List.len list)) }

# orCrash = \result, error ->
#     when result is
#         Ok value -> value
#         Err _ -> crash error
