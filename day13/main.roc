app "advent-2023-roc-day13"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Tile : [Ash, Rocks]

Row : List Tile

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input
    |> Str.trim
    |> parseSurfaces
    |> List.map \surface -> scoreSurface surface 0
    |> List.sum

expect
    result = part1 inputExample1
    result == 405

scoreSurface : List Row, Nat -> Nat
scoreSurface = \rows, targetSmudges ->
    when findReflection rows targetSmudges is
        Ok value -> (value + 1) * 100
        Err NotFound -> scoreTransposed rows targetSmudges

scoreTransposed : List Row, Nat -> Nat
scoreTransposed = \rows, targetSmudges ->
    columns = rows |> transposeRows
    when findReflection columns targetSmudges is
        Ok value -> value + 1
        Err NotFound -> crash "No reflection found"

transposeRows : List Row -> List Row
transposeRows = \rows ->
    firstRow = List.first rows |> okOrCrash "No rows"
    rowIndexes = indexes rows
    List.map (indexes firstRow) \columnIndex ->
        List.map rowIndexes \rowIndex ->
            row = List.get rows rowIndex |> okOrCrash "No row at index \(Num.toStr rowIndex)"
            List.get row columnIndex |> okOrCrash "No tile at index \(Num.toStr columnIndex)"

findReflection : List Row, Nat -> Result Nat [NotFound]
findReflection = \rows, targetSmudges ->
    rows
    |> indexes
    |> List.findFirst \rowIndex ->
        hasReflection { rows, rowIndex, targetSmudges, smudges: 0, depth: 0 }

hasReflection : { rows : List Row, rowIndex : Nat, targetSmudges : Nat, smudges : Nat, depth : Nat } -> Bool
hasReflection = \{ rows, rowIndex, targetSmudges, smudges, depth } ->
    rowResult1 = List.get rows (rowIndex - depth)
    rowResult2 = List.get rows (rowIndex + depth + 1)
    when (rowResult1, rowResult2) is
        (Ok row1, Ok row2) ->
            newSmudges = smudges + (countSmudges row1 row2)
            if newSmudges > targetSmudges then
                Bool.false
            else if depth < rowIndex then
                hasReflection { rows, rowIndex, targetSmudges, smudges: newSmudges, depth: (depth + 1) }
            else
                newSmudges == targetSmudges

        _ ->
            if depth == 0 then
                # Reached the end of the rows and no reflection was found
                Bool.false
            else
                # Reached the end of the rows and a reflection was found
                smudges == targetSmudges

countSmudges : Row, Row -> Nat
countSmudges = \row1, row2 ->
    if row1 == row2 then
        0
    else
        List.map2 row1 row2 \tile1, tile2 ->
            tile1 != tile2
        |> List.countIf \bool -> bool

#
# PART 2
#
part2 : Str -> Nat
part2 = \input ->
    input
    |> Str.trim
    |> parseSurfaces
    |> List.map \surface -> scoreSurface surface 1
    |> List.sum

expect
    result = part2 inputExample1
    result == 400

#
# Parser
#
parseSurfaces : Str -> List (List Row)
parseSurfaces = \input ->
    input
    |> Str.split "\n\n"
    |> List.map parseRows

parseRows : Str -> List Row
parseRows = \input ->
    input
    |> Str.split "\n"
    |> List.map parseRow

parseRow : Str -> List Tile
parseRow = \row ->
    row
    |> Str.graphemes
    |> List.map parseTile

parseTile : Str -> Tile
parseTile = \tile ->
    when tile is
        "." -> Ash
        "#" -> Rocks
        _ -> crash "Invalid tile \(tile)"

#
# Utilities
#
indexes : List a -> List Nat
indexes = \list ->
    List.range { start: At 0, end: Length (Num.toNat (List.len list)) }

okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
