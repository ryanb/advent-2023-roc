app "advent-2023-roc-day14"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

Tile : [Empty, StableRock, RoundRock]

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

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
# PART 2
#
part2 : Str -> Nat
part2 = \input ->
    # We flip the X dimension because we rotate the dish counter-clockwise
    input
    |> parseDish
    |> flipY
    |> cycleDish 1000000000 []
    |> calculateLoadInDish

expect
    result = part2 inputExample1
    result == 64

# Array2D.flipY doesn't work so we make due with this
# See https://github.com/mulias/roc-array2d/issues/2
flipY : Array2D Tile -> Array2D Tile
flipY = \dish ->
    dish
    |> Array2D.rotateCounterClockwise
    |> Array2D.flipX
    |> Array2D.rotateClockwise

cycleDish : Array2D Tile, Nat, List Nat -> Array2D Tile
cycleDish = \dish, times, loadCounts ->
    if times == 0 then
        dish
    else
        newLoadCounts = List.append loadCounts (calculateLoadInDish dish)
        when findPattern newLoadCounts is
            Ok length if times > length -> cycleDish dish (times % length) loadCounts
            _ ->
                dish
                |> rollRocksInDish # North
                |> rollRocksInDish # East (flipped)
                |> rollRocksInDish # South
                |> rollRocksInDish # West (flipped)
                |> cycleDish (times - 1) newLoadCounts

findPattern : List Nat -> Result Nat [NotFound]
findPattern = \values ->
    lastValue = values |> List.last |> orCrash "No values"
    remaining = values |> List.dropLast 1
    when List.findLastIndex remaining \value -> value == lastValue is
        Ok index ->
            length = (List.len remaining) - index
            if length > 1 then
                sublist1 = values |> List.takeLast length
                sublist2 = values |> List.dropLast length |> List.takeLast length
                if sublist1 == sublist2 then
                    Ok length
                else
                    Err NotFound
            else
                Err NotFound

        Err NotFound -> Err NotFound

expect
    result = findPattern [1, 2, 3, 2, 3, 2, 3]
    result == Ok 2

expect
    result = findPattern [1, 2, 3, 4, 2, 3, 4]
    result == Ok 3

expect
    result = findPattern [1, 1, 3, 4, 2, 3, 4]
    result == Err NotFound

rollRocksInDish : Array2D Tile -> Array2D Tile
rollRocksInDish = \dish ->
    dish
    |> Array2D.rotateCounterClockwise
    |> Array2D.toLists
    |> List.map rollRocksInRow
    |> Array2D.fromExactLists
    |> orCrash "Rows are not the same length"

calculateLoadInDish : Array2D Tile -> Nat
calculateLoadInDish = \dish ->
    # We do one more rotation since we calculate by row, not column
    dish
    |> Array2D.rotateCounterClockwise
    |> Array2D.toLists
    |> List.map calculateLoadInRow
    |> List.sum

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
# debugDish = \dish ->
#     dbg Str.concat "\n" (inspectDish dish)

#     dish

# inspectDish = \dish ->
#     dish
#     |> Array2D.map \tile ->
#         when tile is
#             Empty -> "."
#             StableRock -> "#"
#             RoundRock -> "O"
#     |> Array2D.joinWith "" "\n"

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
