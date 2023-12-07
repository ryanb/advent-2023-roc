app "advent-2023-roc-day03"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
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

part1 = \input ->
    input
    |> parseCoords
    |> numbersNearSymbols
    |> List.sum

expect
    result = part1 inputSample
    result == 4361

parseCoords = \input ->
    coords = { numbers: [], symbols: [] }
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.walkWithIndex coords parseRow

parseRow = \coords, row, natY ->
    y = Num.toI32 natY
    scratch = { coords, y, currentNumber: "" }
    Str.graphemes row
    |> List.walkWithIndex scratch parseChar
    |> registerNumber (Num.toI32 (Str.countUtf8Bytes row))
    |> .coords

expect
    coords = { numbers: [], symbols: [] }
    result = parseRow coords "..57..*.." 123
    result
    == {
        numbers: [{ str: "57", fromX: 2, toX: 3, y: 123 }],
        symbols: [{ str: "*", x: 6, y: 123 }],
    }

parseChar = \scratch, char, natX ->
    x = Num.toI32 natX
    if isNumber char then
        { scratch & currentNumber: Str.concat scratch.currentNumber char }
    else
        scratch
        |> registerNumber x
        |> parseSymbol char x

expect
    coords = { numbers: [], symbols: [] }
    scratch = { coords, y: 123, currentNumber: "1" }
    result = parseChar scratch "7" 321
    result == { scratch & currentNumber: "17" }

registerNumber = \scratch, x ->
    if Str.isEmpty scratch.currentNumber then
        scratch
    else
        length = Num.toI32 (Str.countUtf8Bytes scratch.currentNumber)
        number = { str: scratch.currentNumber, fromX: x - length, toX: x - 1, y: scratch.y }
        oldCoords = scratch.coords
        numbers = oldCoords.numbers |> List.append number
        newCoords = { oldCoords & numbers }
        { scratch & coords: newCoords, currentNumber: "" }

expect
    coords = { numbers: [], symbols: [] }
    scratch = { coords, y: 34, currentNumber: "17" }
    result = registerNumber scratch 12
    result.currentNumber == "" && result.coords.numbers == [{ str: "17", fromX: 10, toX: 11, y: 34 }]

parseSymbol = \scratch, char, x ->
    if char == "." then
        scratch
    else
        symbol = { str: char, x, y: scratch.y }
        oldCoords = scratch.coords
        symbols = oldCoords.symbols |> List.append symbol
        newCoords = { oldCoords & symbols }
        { scratch & coords: newCoords }

expect
    coords = { numbers: [], symbols: [] }
    scratch = { coords, y: 34, currentNumber: "" }
    result = parseSymbol scratch "*" 12
    result.coords.symbols == [{ str: "*", x: 12, y: 34 }]

isNumber = \grapheme ->
    List.range { start: At 0, end: At 9 }
    |> List.map Num.toStr
    |> List.contains grapheme

numbersNearSymbols = \coords ->
    coords.numbers
    |> List.keepIf \number -> isNearSymbol number coords
    |> List.map .str
    |> List.keepOks Str.toI32

isNearSymbol = \number, coords ->
    fromX = number.fromX - 1
    toX = number.toX + 1
    fromY = number.y - 1
    toY = number.y + 1
    List.any coords.symbols \symbol ->
        symbol.x >= fromX && symbol.x <= toX && symbol.y >= fromY && symbol.y <= toY

expect
    number = { str: "123", fromX: 0, toX: 2, y: 1 }
    symbol = { str: "*", x: 3, y: 0 }
    coords = { numbers: [number], symbols: [symbol] }
    result = isNearSymbol number coords
    result == Bool.true

expect
    number = { str: "123", fromX: 0, toX: 2, y: 1 }
    symbol = { str: "*", x: 4, y: 0 }
    coords = { numbers: [number], symbols: [symbol] }
    result = isNearSymbol number coords
    result == Bool.false

part2 = \input ->
    input
    |> parseCoords
    |> gearRatios
    |> List.sum

expect
    result = part2 inputSample
    result == 467835

gearRatios = \coords ->
    coords.symbols
    |> List.keepIf \symbol -> symbol.str == "*"
    |> List.map \symbol -> nearbyNumbers symbol coords
    |> List.keepIf \numbers -> List.len numbers == 2
    |> List.map multiplyNumbers

nearbyNumbers = \symbol, coords ->
    fromX = symbol.x - 1
    toX = symbol.x + 1
    fromY = symbol.y - 1
    toY = symbol.y + 1
    List.keepIf coords.numbers \number ->
        number.toX >= fromX && number.fromX <= toX && number.y >= fromY && number.y <= toY

multiplyNumbers = \numbers ->
    numbers
    |> List.map .str
    |> List.keepOks Str.toI32
    |> List.walk 1 \a, b -> a * b
