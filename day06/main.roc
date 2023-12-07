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
        parser.String.{ parseStr, string, digits },
        parser.Core.{ apply, const, skip, maybe, oneOrMore, sepBy },
    ]
    provides [main] to pf

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 = \input ->
    input
    |> parseRaces
    |> countWays
    |> List.walk 1 Num.mul

expect
    result = part1 inputSample
    result == 288

parseRaces = \input ->
    when parseStr racesParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

racesParser =
    spaces = oneOrMore (string " ")
    const (\times -> \distances -> { times, distances })
    |> skip (string "Time:")
    |> skip spaces
    |> apply (digits |> sepBy spaces)
    |> skip (string "\n")
    |> skip (string "Distance:")
    |> skip spaces
    |> apply (digits |> sepBy spaces)
    |> skip (maybe (string "\n"))

countWays = \races ->
    races.times
    |> List.mapWithIndex \time, index ->

        distance =
            when List.get races.distances index is
                Ok result -> result
                Err OutOfBounds -> crash "Missing distance for \(Num.toStr index)"
        countWaysForRace time distance

countWaysForRace = \time, distance ->
    # We need to use Float and not Frac here since Frac doesn't have Num.ceiling implemented
    optimalDuration = Num.ceiling ((Num.toF32 time) / 2.0)

    evenOffset =
        if Num.isOdd time then
            0
        else
            1

    firstDuration =
        List.range { start: At 1, end: At optimalDuration }
        |> List.findFirst \duration ->
            tempDistance = duration * (time - duration)
            tempDistance > distance

    when firstDuration is
        Ok duration -> (optimalDuration - duration) * 2 + evenOffset
        Err NotFound -> crash "Unable to find dration that beats \(Num.toStr distance)"

expect
    result = countWaysForRace 7 9
    result == 4

expect
    result = countWaysForRace 30 200
    result == 9
