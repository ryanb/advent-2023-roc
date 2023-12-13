app "advent-2023-roc-day11"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Galaxy : (I64, I64)

Galaxies : List Galaxy

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 : Str -> I64
part1 = \input ->
    input
    |> parseGalaxies
    |> expandGalaxies
    |> pairGalaxies
    |> List.map distance
    |> List.sum

expect
    result = part1 inputExample1
    result == 374

expandGalaxies : Galaxies -> Galaxies
expandGalaxies = \galaxies ->
    xOffsets = galaxies |> List.map (\(x, _y) -> x) |> offsets
    yOffsets = galaxies |> List.map (\(_x, y) -> y) |> offsets
    List.map galaxies \(x, y) ->
        xOffset = xOffsets |> Dict.get x |> okOrCrash "Unable to find x coordinate"
        yOffset = yOffsets |> Dict.get y |> okOrCrash "Unable to find y coordinate"
        (x + xOffset, y + yOffset)

expect
    galaxies = [(0, 0), (2, 2), (4, 4)]
    result = expandGalaxies galaxies
    result == [(0, 0), (3, 3), (6, 6)]

offsets : List I64 -> Dict I64 I64
offsets = \numbers ->
    state = { offset: 0i64, dict: Dict.empty {} }
    min = List.min numbers |> okOrCrash "Unable to find min"
    max = List.max numbers |> okOrCrash "Unable to find max"
    List.range { start: At min, end: At max }
    |> List.walk state \{ offset, dict }, number ->
        if numbers |> List.contains number then
            { offset, dict: Dict.insert dict number offset }
        else
            { offset: offset + 1, dict }
    |> .dict

pairGalaxies : Galaxies -> List (Galaxy, Galaxy)
pairGalaxies = \galaxies ->
    state = { pairs: [], remaining: galaxies }
    galaxies
    |> List.walk state \state2, galaxy ->
        remaining = state2.remaining |> List.dropFirst 1
        remaining
        |> List.walk state2 \state3, other ->
            pairs = state3.pairs |> List.append (galaxy, other)
            { pairs, remaining }
    |> .pairs

expect
    galaxies = [(0, 0), (1, 1), (2, 2)]
    result = pairGalaxies galaxies
    result == [((0, 0), (1, 1)), ((0, 0), (2, 2)), ((1, 1), (2, 2))]

distance : (Galaxy, Galaxy) -> I64
distance = \((x1, y1), (x2, y2)) ->
    Num.abs (x2 - x1) + Num.abs (y2 - y1)

#
# Parser
#
parseGalaxies : Str -> Galaxies
parseGalaxies = \input ->
    input
    |> Str.split "\n"
    |> List.walkWithIndex [] parseRow

expect
    expectedGalaxies = [(0, 0), (2, 1), (1, 2)]
    result = parseGalaxies "#..\n..#\n.#."
    result == expectedGalaxies

parseRow : Galaxies, Str, Nat -> Galaxies
parseRow = \galaxies, row, y ->
    row
    |> Str.graphemes
    |> List.walkWithIndex galaxies \list, char, x ->
        if char == "#" then
            list |> List.append (Num.toI64 x, Num.toI64 y)
        else
            list

#
# Utilities
#
okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
