app "advent-2023-roc-day13"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

Tile : [Empty, StableRock, RoundRock]

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input |> parseDish |> rollRocks |> List.map calculateLoadInRow |> List.sum

expect
    result = part1 inputExample1
    result == 136

rollRocks : Array2D Tile -> List (List Tile)
rollRocks = \dish ->
    dish
    |> Array2D.rotateCounterClockwise
    |> Array2D.toLists
    |> List.map rollRocksInRow

rollRocksInRow : List Tile -> List Tile
rollRocksInRow = \row ->
    row
    |> List.walkWithIndex row \newRow, tile, index ->
        when tile is
            RoundRock -> rollRock newRow index
            _ -> newRow

expect
    result = rollRocksInRow [Empty, RoundRock, Empty, RoundRock]
    result == [RoundRock, RoundRock, Empty, Empty]

rollRock : List Tile, Nat -> List Tile
rollRock = \row, index ->
    if index == 0 then
        row
    else
        newIndex = rollIndex row index
        row |> List.set index Empty |> List.set newIndex RoundRock

expect
    result = rollRock [Empty, RoundRock, Empty, RoundRock] 3
    result == [Empty, RoundRock, RoundRock, Empty]

rollIndex : List Tile, Nat -> Nat
rollIndex = \row, index ->
    List.range { start: At (index - 1), end: At 0 }
    |> List.walkUntil 0 \_state, newIndex ->
        tile = row |> List.get newIndex |> orCrash "Unexpected index \(Num.toStr newIndex)"
        if tile == Empty then
            Continue 0
        else
            Break (newIndex + 1)

expect
    result = rollIndex [Empty, Empty, Empty, RoundRock, Empty] 3
    result == 0

expect
    result = rollIndex [Empty, RoundRock, Empty, RoundRock] 3
    result == 2

calculateLoadInRow : List Tile -> Nat
calculateLoadInRow = \row ->
    length = List.len row
    List.walkWithIndex row 0 \load, tile, index ->
        when tile is
            RoundRock -> load + (length - index)
            _ -> load

#
# Parser
#
parseDish : Str -> Array2D Tile
parseDish = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseRow
    |> Array2D.fromExactLists
    |> orCrash "Invalid input: Rows are not the same length"

parseRow : Str -> List Tile
parseRow = \row ->
    row
    |> Str.graphemes
    |> List.map parseTile

parseTile : Str -> Tile
parseTile = \tile ->
    when tile is
        "." -> Empty
        "#" -> StableRock
        "O" -> RoundRock
        _ -> crash "Invalid tile \(tile)"

#
# Utilities
#
# debug = \value ->
#     dbg value

#     value

# indexes : List a -> List Nat
# indexes = \list ->
#     List.range { start: At 0, end: Length (Num.toNat (List.len list)) }

orCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
