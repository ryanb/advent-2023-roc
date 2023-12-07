app "advent-2023-roc-day05"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
        "input-sample-alt.txt" as inputSampleAlt : Str,
        parser.String.{ parseStr, string, digits },
        parser.Core.{ apply, const, skip, maybe, chompUntil, sepBy },
    ]
    provides [main] to pf

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

cardValues =
    "23456789TJQKA"
    |> Str.toScalars
    |> List.map Num.toU64
    |> List.walkWithIndex (Dict.empty {}) \dict, card, index ->
        Dict.insert dict card (Num.toU64 index)

#
# PART 1
#
part1 = \input ->
    input
    |> parseHands
    |> List.map generateHandStrength
    |> sortBy .strength
    |> List.mapWithIndex \hand, index -> hand.bid * (index + 1)
    |> List.sum

expect
    result = part1 inputSample
    result == 6440

# From https://www.reddit.com/r/adventofcode/comments/18cr4xr/2023_day_7_better_example_input_not_a_spoiler/
expect
    result = part1 inputSampleAlt
    result == 6592

parseHands = \input ->
    when parseStr handsParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

handsParser =
    handParser
    |> sepBy (string "\n")
    |> skip (maybe (string "\n"))

handParser =
    const (\cards -> \bid -> { cards: cards, bid })
    |> apply (chompUntil ' ')
    |> skip (string " ")
    |> apply digits

generateHandStrength = \hand ->
    strength = handStrength (hand.cards |> List.map Num.toU64)
    # cardsStr = hand.cards |> Str.fromUtf8 |> okOrCrash "Unable to convert cards back to string"
    { strength, bid: hand.bid }

handStrength = \cards ->
    cards
    |> List.map cardValue
    |> List.prepend (handTypeValue cards)
    |> List.reverse
    |> List.walkWithIndex 0 \sum, value, index ->
        sum + value * (Num.powInt 16 (Num.toU64 index))

cardValue = \card ->
    (Dict.get cardValues card) |> okOrCrash "Invalid card \(Num.toStr card)"

handTypeValue = \cards ->
    when handTypeCounts cards is
        [1, 1, 1, 1, 1] -> 0
        [2, 1, 1, 1] -> 1
        [2, 2, 1] -> 2
        [3, 1, 1] -> 3
        [3, 2] -> 4
        [4, 1] -> 5
        [5] -> 6
        _ -> crash "Invalid poker hand"

handTypeCounts = \cards ->
    cards
    |> List.walk (Dict.empty {}) \dict, card ->
        when Dict.get dict card is
            Ok amount -> Dict.insert dict card (amount + 1)
            Err KeyNotFound -> Dict.insert dict card 1
    |> Dict.values
    |> List.sortDesc

expect
    result = handTypeCounts ("AAAAA" |> Str.toScalars)
    result == [5]

expect
    result = handTypeCounts ("KKAAA" |> Str.toScalars)
    result == [3, 2]

expect
    result = handTypeCounts ("AKAQA" |> Str.toScalars)
    result == [3, 1, 1]

expect
    result = handTypeCounts ("AKQJT" |> Str.toScalars)
    result == [1, 1, 1, 1, 1]

sortBy = \list, func ->
    List.sortWith list \a, b ->
        aVal = func a
        bVal = func b
        if aVal < bVal then
            LT
        else if aVal > bVal then
            GT
        else
            EQ

okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
