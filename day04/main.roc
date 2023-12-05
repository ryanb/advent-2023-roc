app "advent-2023-roc-day04"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
        parser.String.{ digits, parseStr, string },
        parser.Core.{ apply, const, skip, maybe, oneOrMore, sepBy },
    ]
    provides [main] to pf

main =
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"
# _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
# Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseCard
    |> List.map scoreCard
    |> List.sum

expect
    result = part1 inputSample
    result == 13


parseCard = \str ->
    when parseStr cardParser str is
        Ok parsed -> parsed
        _ -> crash "Invalid card \(str)"

expect
    result = parseCard "Card 1: 1 22 333 | 1 22"
    result
    == {
        id: 1,
        winningNumbers: [1, 22, 333],
        givenNumbers: [1, 22],
    }


cardParser =
    spaces = oneOrMore (string " ")
    const (\id -> \winningNumbers -> \givenNumbers -> { id, winningNumbers, givenNumbers })
    |> skip (string "Card")
    |> skip spaces
    |> apply digits
    |> skip (string ":")
    |> skip spaces
    |> apply (digits |> sepBy spaces)
    |> skip (string " |")
    |> skip spaces
    |> apply (digits |> sepBy spaces)
    |> skip (maybe (string "\n"))


scoreCard = \card ->
    count =
        card
        |> matchingNumbers
        |> List.len
    if count == 0 then
        0
    else
        Num.powInt 2 (count - 1)

expect
    result = scoreCard { id: 1, winningNumbers: [1, 2], givenNumbers: [1, 2, 3] }
    result == 2


matchingNumbers = \card ->
    card.givenNumbers
    |> List.keepIf \number ->
        card.winningNumbers |> List.contains number

expect
    result = matchingNumbers { id: 1, winningNumbers: [1, 2], givenNumbers: [2, 3] }
    result == [2]
