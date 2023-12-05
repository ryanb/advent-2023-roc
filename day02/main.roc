app "advent-2023-roc-day02"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
    ]
    provides [main] to pf

maxDict =
    Dict.empty {}
    |> Dict.insert "red" 12
    |> Dict.insert "green" 13
    |> Dict.insert "blue" 14

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

part1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseGame
    |> List.keepIf possibleGame
    |> List.map .id
    |> List.sum

expect
    result = part1 inputSample
    result == 8

parseGame = \str ->
    { before: gameIdStr, after: handfullsStr } =
        when Str.splitFirst str ": " is
            Ok result -> result
            Err _ -> crash "Unable to parse \(str)"
    { id: parseGameId gameIdStr, handfulls: parseHandfulls handfullsStr }

parseGameId = \str ->
    idStr =
        when Str.splitFirst str " " is
            Ok { after } -> after
            Err _ -> crash "Unable to parse \(str)"
    when Str.toI32 idStr is
        Ok id -> id
        Err _ -> crash "Unable to convert \(idStr) to string"

expect
    result = parseGameId "Game 4"
    result == 4

parseHandfulls = \str ->
    str
    |> Str.split "; "
    |> List.map parseCubeTypes

expect
    result = parseCubeTypes "3 red, 5 green"
    result == [{ color: "red", amount: 3 }, { color: "green", amount: 5 }]

parseCubeTypes = \str ->
    str
    |> Str.split ", "
    |> List.map parseCubeType

expect
    result = parseCubeTypes "3 red, 5 green"
    result == [{ color: "red", amount: 3 }, { color: "green", amount: 5 }]

parseCubeType = \str ->
    { before: amountStr, after: color } =
        when Str.splitFirst str " " is
            Ok result -> result
            Err _ -> crash "Unable to parse color \(str)"
    amount =
        when Str.toI32 amountStr is
            Ok result -> result
            Err _ -> crash "Unable to convert \(amountStr) to number"
    { color, amount }

expect
    result = parseCubeType "3 red"
    result == { color: "red", amount: 3 }

possibleGame = \game -> possibleHandfulls game.handfulls

possibleHandfulls = \handfulls -> handfulls |> List.all possibleCubeTypes

possibleCubeTypes = \cubeTypes -> cubeTypes |> List.all possibleCubeType

possibleCubeType = \{ color, amount } ->
    when Dict.get maxDict color is
        Ok max -> max >= amount
        Err KeyNotFound -> crash "Unknown max for color: \(color)"

expect
    result = possibleCubeType { color: "red", amount: 3 }
    result == Bool.true

expect
    result = possibleCubeType { color: "red", amount: 100 }
    result == Bool.false

part2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseGame
    |> List.map maxAmounts
    |> List.map multiplyAmounts
    |> List.sum

expect
    result = part2 inputSample
    result == 2286

maxAmounts = \game ->
    amounts = { red: 0, green: 0, blue: 0 }
    List.walk game.handfulls amounts \handfullAmounts, cubeTypes ->
        List.walk cubeTypes handfullAmounts \cubeTypeAmounts, cubeType ->
            maxAmount cubeTypeAmounts cubeType

expect
    game = { handfulls: [[{ color: "red", amount: 5 }], [{ color: "red", amount: 3 }]] }
    result = maxAmounts game
    result == { red: 5, green: 0, blue: 0 }

maxAmount = \amounts, { color, amount } ->
    when color is
        "red" ->
            if amount > amounts.red then
                { amounts & red: amount }
            else
                amounts

        "green" ->
            if amount > amounts.green then
                { amounts & green: amount }
            else
                amounts

        "blue" ->
            if amount > amounts.blue then
                { amounts & blue: amount }
            else
                amounts

        _ -> crash "Unexpected color: \(color)"

expect
    result = maxAmount { red: 7, green: 0, blue: 0 } { color: "red", amount: 8 }
    result == { red: 8, green: 0, blue: 0 }

multiplyAmounts = \{ red, green, blue } -> red * green * blue
