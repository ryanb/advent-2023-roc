app "advent-2023-roc-day05"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
        parser.String.{ parseStr, string, digits },
        parser.Core.{ apply, const, skip, maybe, oneOf, sepBy },
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 = \input ->
    locations = input |> parseAlmanac |> seedLocations
    when List.min locations is
        Ok result -> result
        Err ListWasEmpty -> crash "No locations found"

expect
    result = part1 inputSample
    result == 35

parseAlmanac = \input ->
    when parseStr almanacParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

almanacParser =
    const (\seeds -> \tables -> { seeds, tables })
    |> skip (string "seeds: ")
    |> apply (digits |> sepBy (string " "))
    |> skip (string "\n\n")
    |> apply (tableParser |> sepBy (string "\n\n"))
    |> skip (maybe (string "\n"))

expect
    input = "seeds: 1 22 333\n\nseed-to-soil map:\n3 22 111\n\nwater-to-light map:\n4 5 6"
    result = parseStr almanacParser input
    result
    == Ok {
        seeds: [1, 22, 333],
        tables: [
            { label: SeedToSoil, rows: [{ destination: 3, source: 22, length: 111 }] },
            { label: WaterToLight, rows: [{ destination: 4, source: 5, length: 6 }] },
        ],
    }

tableParser =
    const (\label -> \rows -> { label, rows })
    |> apply (oneOf labelParsers)
    |> skip (string " map:\n")
    |> apply (rowParser |> sepBy (string "\n"))

expect
    result = parseStr tableParser "seed-to-soil map:\n3 22 111\n4 5 6"
    result
    == Ok {
        label: SeedToSoil,
        rows: [
            { destination: 3, source: 22, length: 111 },
            { destination: 4, source: 5, length: 6 },
        ],
    }

labelParsers = [
    const SeedToSoil |> skip (string "seed-to-soil"),
    const SoilToFertilizer |> skip (string "soil-to-fertilizer"),
    const FertilizerToWater |> skip (string "fertilizer-to-water"),
    const WaterToLight |> skip (string "water-to-light"),
    const LightToTemperature |> skip (string "light-to-temperature"),
    const TemperatureToHumidity |> skip (string "temperature-to-humidity"),
    const HumidityToLocation |> skip (string "humidity-to-location"),
]

rowParser =
    const (\destination -> \source -> \length -> { destination, source, length })
    |> apply digits
    |> skip (string " ")
    |> apply digits
    |> skip (string " ")
    |> apply digits

expect
    result = parseStr rowParser "55 7 3"
    result == Ok { destination: 55, source: 7, length: 3 }

seedLocations = \almanac ->
    almanac.seeds |> List.map \seed -> seedLocation seed almanac.tables

seedLocation = \seed, tables ->
    tables |> List.walk seed applyTable

applyTable = \value, table ->
    table.rows
    |> List.walkUntil value \_value, row ->
        when applyRow value row is
            Ok result -> Break result
            Err OutOfRange -> Continue value

applyRow = \value, row ->
    if value >= row.source && value <= row.source + row.length - 1 then
        Ok (value - row.source + row.destination)
    else
        Err OutOfRange

expect
    row = { destination: 55, source: 5, length: 5 }
    result = applyRow 1 row
    result == Err OutOfRange

expect
    row = { destination: 55, source: 5, length: 5 }
    result = applyRow 10 row
    result == Err OutOfRange

expect
    row = { destination: 55, source: 5, length: 5 }
    result = applyRow 9 row
    result == Ok 59

#
# PART 2
#
part2 = \input ->
    almanac = input |> parseAlmanac
    ranges = seedRanges almanac.seeds
    locations = almanac.tables |> List.walk ranges processTable |> List.map .from
    when List.min locations is
        Ok result -> result
        Err ListWasEmpty -> crash "No locations found"

expect
    result = part2 inputSample
    result == 46

seedRanges = \seeds ->
    seeds |> List.chunksOf 2 |> List.map seedRange

seedRange = \seeds ->
    when seeds is
        [from, length] -> { from, to: from + length - 1 }
        _ -> crash "Unexpected number of seeds"

processTable = \ranges, table ->
    newQueue = table.rows |> List.walk { current: ranges, next: [] } processRow
    List.concat newQueue.current newQueue.next

processRow = \queue, row ->
    toRange = { from: row.source, to: row.source + row.length - 1 }
    queue.current
    |> List.walk { current: [], next: queue.next } \newQueue, fromRange ->
        { inside, outside } = fitRange fromRange toRange
        if List.isEmpty inside then
            { current: List.concat newQueue.current outside, next: newQueue.next }
        else
            next = convertRanges inside row
            { current: List.concat newQueue.current outside, next: List.concat newQueue.next next }

fitRange = \fromRange, toRange ->
    if fromRange.to < toRange.from || fromRange.from > toRange.to then
        # Fully outside
        { inside: [], outside: [fromRange] }
    else if fromRange.from >= toRange.from && fromRange.to <= toRange.to then
        # Fully inside
        { inside: [fromRange], outside: [] }
    else if fromRange.from < toRange.from && fromRange.to <= toRange.to then
        # Overlapping left side
        outside = [{ from: fromRange.from, to: toRange.from - 1 }]
        inside = [{ from: toRange.from, to: fromRange.to }]
        { outside, inside }
    else if fromRange.from >= toRange.from && fromRange.to > toRange.to then
        # Overlapping right side
        inside = [{ from: fromRange.from, to: toRange.to }]
        outside = [{ from: toRange.to + 1, to: fromRange.to }]
        { inside, outside }
    else
        # Overlapping both sides
        inside = [{ from: toRange.from, to: toRange.to }]
        outside = [
            { from: fromRange.from, to: toRange.from - 1 },
            { from: toRange.to + 1, to: fromRange.to },
        ]
        { inside, outside }

# Fully outside left
expect
    result = fitRange { from: 10, to: 20 } { from: 30, to: 40 }
    result == { inside: [], outside: [{ from: 10, to: 20 }] }

# Fully outside right
expect
    result = fitRange { from: 30, to: 40 } { from: 10, to: 20 }
    result == { inside: [], outside: [{ from: 30, to: 40 }] }

# Fully inside
expect
    result = fitRange { from: 10, to: 20 } { from: 5, to: 25 }
    result == { inside: [{ from: 10, to: 20 }], outside: [] }

# Overlapping left side
expect
    result = fitRange { from: 10, to: 30 } { from: 20, to: 40 }
    result == { inside: [{ from: 20, to: 30 }], outside: [{ from: 10, to: 19 }] }

# Overlapping right side
expect
    result = fitRange { from: 20, to: 40 } { from: 10, to: 30 }
    result == { inside: [{ from: 20, to: 30 }], outside: [{ from: 31, to: 40 }] }

# Overlapping both sides
expect
    result = fitRange { from: 10, to: 40 } { from: 20, to: 30 }
    result
    == {
        inside: [{ from: 20, to: 30 }],
        outside: [{ from: 10, to: 19 }, { from: 31, to: 40 }],
    }

convertRanges = \ranges, row ->
    ranges
    |> List.map \range -> {
        from: range.from - row.source + row.destination,
        to: range.to - row.source + row.destination,
    }
