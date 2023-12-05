app "advent-2023-roc-day03"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
    ]
    provides [main] to pf

main =
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

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
        numbers: [{ str: "57", x: 2, y: 123 }],
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
        number = { str: scratch.currentNumber, x: x - length, y: scratch.y }
        oldCoords = scratch.coords
        numbers = oldCoords.numbers |> List.append number
        newCoords = { oldCoords & numbers }
        { scratch & coords: newCoords, currentNumber: "" }

expect
    coords = { numbers: [], symbols: [] }
    scratch = { coords, y: 34, currentNumber: "17" }
    result = registerNumber scratch 12
    result.currentNumber == "" && result.coords.numbers == [{ str: "17", x: 10, y: 34 }]

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
    fromX = number.x - 1
    toX = number.x + (Num.toI32 (Str.countUtf8Bytes number.str))
    fromY = number.y - 1
    toY = number.y + 1
    List.any coords.symbols \symbol ->
        symbol.x >= fromX && symbol.x <= toX && symbol.y >= fromY && symbol.y <= toY

expect
    number = { str: "123", x: 0, y: 1 }
    symbol = { str: "*", x: 4, y: 0 }
    coords = { numbers: [number], symbols: [symbol] }
    result = isNearSymbol number coords
    result == Bool.true

expect
    number = { str: "123", x: 0, y: 1 }
    symbol = { str: "*", x: 6, y: 0 }
    coords = { numbers: [number], symbols: [symbol] }
    result = isNearSymbol number coords
    result == Bool.false
