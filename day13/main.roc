app "advent-2023-roc-day13"
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

Tile : [Ash, Rocks]

Row : List Tile

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input
    |> Str.trim
    |> parseSurfaces
    |> List.map scoreSurface
    |> List.sum

expect
    result = part1 inputExample1
    result == 405

scoreSurface : List Row -> Nat
scoreSurface = \rows ->
    when findReflection rows is
        Ok value -> (value + 1) * 100
        Err NotFound -> scoreTransposed rows

scoreTransposed : List Row -> Nat
scoreTransposed = \rows ->
    columns = rows |> transposeRows
    when findReflection columns is
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

findReflection : List Row -> Result Nat [NotFound]
findReflection = \rows ->
    rows
    |> indexes
    |> List.findFirst \rowIndex ->
        hasReflection rows rowIndex 0

hasReflection : List Row, Nat, Nat -> Bool
hasReflection = \rows, rowIndex, depth ->
    rowResult1 = List.get rows (rowIndex - depth)
    rowResult2 = List.get rows (rowIndex + depth + 1)
    when (rowResult1, rowResult2) is
        (Ok row1, Ok row2) ->
            if row1 == row2 then
                if depth < rowIndex then
                    hasReflection rows rowIndex (depth + 1)
                else
                    Bool.true
            else
                Bool.false

        _ ->
            if depth == 0 then
                # Reached the end of the row and no reflection was found
                Bool.false
            else
                # Reached the end of the row and a reflection was found
                Bool.true

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
