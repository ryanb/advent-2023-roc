app "advent-2023-roc-day10"
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

Direction : [North, South, East, West]

Position : (I64, I64)

Tile : [Ground, Start, VerticalPipe, HorizontalPipe, NorthEastPipe, NorthWestPipe, SouthWestPipe, SouthEastPipe]

Tiles : Dict Position Tile

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 : Str -> Int *
part1 = \input ->
    input
    |> parseTiles
    |> pipeLength
    |> Num.divTrunc 2

expect
    result = part1 inputExample1
    result == 8

pipeLength : Tiles -> Int *
pipeLength = \tiles ->
    position = startPosition tiles |> okOrCrash "Unable to find start position"
    recursivePipeLength tiles position Start (-1, -1) 0

startPosition : Tiles -> Result Position [NotFound]
startPosition = \tiles ->
    tiles
    |> Dict.walkUntil (Err NotFound) \result, position, tile ->
        if tile == Start then
            Break (Ok position)
        else
            Continue result

recursivePipeLength : Tiles, Position, Tile, Position, Int a -> Int a
recursivePipeLength = \tiles, position, tile, previousPosition, depth ->
    result = findConnectedPositionAndTile tiles position tile previousPosition
    when result is
        Ok (nextPosition, nextTile) -> recursivePipeLength tiles nextPosition nextTile position (depth + 1)
        Err Start -> depth + 1 # We've reached the end of the pipe
        Err NotFound -> crash "Incomplete loop"

findConnectedPositionAndTile : Tiles, Position, Tile, Position -> Result (Position, Tile) [Start, NotFound]
findConnectedPositionAndTile = \tiles, position, tile, previousPosition ->
    connectionDirections tile
    |> List.walkUntil (Err NotFound) \result, direction ->
        nextPosition = adjacentPosition position direction
        if nextPosition == previousPosition then
            Continue result
        else
            when Dict.get tiles nextPosition is
                Ok nextTile ->
                    if nextTile == Start then
                        Break (Err Start)
                    else if isConnected direction nextTile then
                        Break (Ok (nextPosition, nextTile))
                    else
                        Continue result

                Err _ -> Continue result # Out of bounds

connectionDirections : Tile -> List Direction
connectionDirections = \tile ->
    when tile is
        Start -> [North, South, East, West]
        HorizontalPipe -> [East, West]
        VerticalPipe -> [North, South]
        NorthEastPipe -> [North, East]
        NorthWestPipe -> [North, West]
        SouthEastPipe -> [South, East]
        SouthWestPipe -> [South, West]
        _ -> []

oppositeDirection : Direction -> Direction
oppositeDirection = \direction ->
    when direction is
        North -> South
        South -> North
        East -> West
        West -> East

adjacentPosition : Position, Direction -> Position
adjacentPosition = \(x, y), direction ->
    when direction is
        North -> (x, y - 1)
        South -> (x, y + 1)
        East -> (x + 1, y)
        West -> (x - 1, y)

isConnected : Direction, Tile -> Bool
isConnected = \direction, tile ->
    connectionDirections tile
    |> List.contains (oppositeDirection direction)

#
# Parser
#
parseTiles : Str -> Tiles
parseTiles = \input ->
    input
    |> Str.split "\n"
    |> List.walkWithIndex (Dict.empty {}) parseRowTiles

expect
    tiles = parseTiles "S-7\n|.|\nL-J\n"
    expectedTiles = [
        ((0, 0), Start),
        ((1, 0), HorizontalPipe),
        ((2, 0), SouthWestPipe),
        ((0, 1), VerticalPipe),
        ((1, 1), Ground),
        ((2, 1), VerticalPipe),
        ((0, 2), NorthEastPipe),
        ((1, 2), HorizontalPipe),
        ((2, 2), NorthWestPipe),
    ]
    Dict.toList tiles == expectedTiles

parseRowTiles : Tiles, Str, Nat -> Tiles
parseRowTiles = \tiles, row, y ->
    row
    |> Str.graphemes
    |> List.map parseTile
    |> List.walkWithIndex tiles \dict, tile, x -> Dict.insert dict (Num.toI64 x, Num.toI64 y) tile

parseTile : Str -> Tile
parseTile = \tile ->
    when tile is
        "." -> Ground
        "S" -> Start
        "|" -> VerticalPipe
        "-" -> HorizontalPipe
        "L" -> NorthEastPipe
        "J" -> NorthWestPipe
        "7" -> SouthWestPipe
        "F" -> SouthEastPipe
        _ -> crash "Invalid tile \(tile)"

#
# Utilities
#
okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
