app "advent-2023-roc-day16"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
        array2d.Array2D.{ Array2D },
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Tile : [Empty, SplitVertical, SplitHorizontal, DiagonalForward, DiagonalBackward]
Point : { x : Nat, y : Nat }
Direction : [North, East, South, West]
Beam : { location : Point, direction : Direction }

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input
    |> parseTiles
    |> startBeam
    |> Set.map \{ location } -> location
    |> Set.len

expect
    result = part1 inputExample1
    result == 46

startBeam : Array2D Tile -> Set Beam
startBeam = \tiles ->
    beam = { location: { x: 0, y: 0 }, direction: East }
    recursiveBeams { tiles, beams: [beam], pastBeams: Set.empty {} }

recursiveBeams : { tiles : Array2D Tile, beams : List Beam, pastBeams : Set Beam } -> Set Beam
recursiveBeams = \{ tiles, beams, pastBeams } ->
    newPastBeams =
        beams
        |> List.walk pastBeams \accPastBeams, beam ->
            accPastBeams |> Set.insert beam
    newBeams =
        beams
        |> List.walk [] \accBeams, beam -> addNextBeam { tiles, newBeams: accBeams, beam }
        |> List.dropIf \beam -> pastBeams |> Set.contains beam
    if List.isEmpty newBeams then
        newPastBeams
    else
        recursiveBeams { tiles, beams: newBeams, pastBeams: newPastBeams }

addNextBeam : { tiles : Array2D Tile, newBeams : List Beam, beam : Beam } -> List Beam
addNextBeam = \{ tiles, newBeams, beam } ->
    { x, y } = beam.location
    # We flip x and y here since it's flipped in Array2D for some reason
    shape = Array2D.shape tiles
    dimensions = { x: shape.dimY, y: shape.dimX }
    when tiles |> Array2D.get { x: y, y: x } is
        Ok SplitVertical ->
            List.concat newBeams (nextSplitBeams { beam, tile: SplitVertical, dimensions })

        Ok SplitHorizontal ->
            List.concat newBeams (nextSplitBeams { beam, tile: SplitHorizontal, dimensions })

        Ok tile ->
            when nextSingleBeam { beam, tile, dimensions } is
                Ok newBeam -> List.append newBeams newBeam
                Err OutOfBounds -> newBeams

        Err OutOfBounds -> crash "Beam out of bounds"

nextSingleBeam : { beam : Beam, tile : Tile, dimensions : Point } -> Result Beam [OutOfBounds]
nextSingleBeam = \{ beam, tile, dimensions } ->
    direction = continueDirection tile beam.direction
    nextLocation beam.location direction dimensions
    |> Result.map \location -> { location, direction }

continueDirection : Tile, Direction -> Direction
continueDirection = \tile, direction ->
    when tile is
        Empty -> direction
        DiagonalForward ->
            when direction is
                North -> East
                East -> North
                South -> West
                West -> South

        DiagonalBackward ->
            when direction is
                North -> West
                East -> South
                South -> East
                West -> North

        _ -> crash "Unexpected tile"

nextLocation : Point, Direction, Point -> Result Point [OutOfBounds]
nextLocation = \{ x, y }, direction, dimensions ->
    when direction is
        North if y > 0 ->
            Ok { x, y: y - 1 }

        East if x < dimensions.x - 1 ->
            Ok { x: x + 1, y }

        South if y < dimensions.y - 1 ->
            Ok { x, y: y + 1 }

        West if x > 0 ->
            Ok { x: x - 1, y }

        _ -> Err OutOfBounds

nextSplitBeams : { beam : Beam, tile : Tile, dimensions : Point } -> List Beam
nextSplitBeams = \{ beam, tile, dimensions } ->
    splitDirections tile beam.direction
    |> List.keepOks \direction ->
        nextLocation beam.location direction dimensions
        |> Result.map \location -> { location, direction }

splitDirections : Tile, Direction -> List Direction
splitDirections = \tile, direction ->
    when tile is
        SplitVertical ->
            when direction is
                East -> [North, South]
                West -> [North, South]
                _ -> [direction]

        SplitHorizontal ->
            when direction is
                North -> [East, West]
                South -> [East, West]
                _ -> [direction]

        _ -> crash "Unexpected tile"

#
# Parser
#
parseTiles : Str -> Array2D Tile
parseTiles = \input ->
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
        "|" -> SplitVertical
        "-" -> SplitHorizontal
        "/" -> DiagonalForward
        "\\" -> DiagonalBackward
        _ -> crash "Invalid tile \(tile)"

#
# PART 2
#
part2 : Str -> Nat
part2 = \input ->
    tiles = input |> parseTiles
    startingBeams tiles
    |> List.map \beam -> recursiveBeams { tiles, beams: [beam], pastBeams: Set.empty {} }
    |> List.map energizedTilesCount
    |> List.max
    |> orCrash "No beams found"

startingBeams : Array2D Tile -> List Beam
startingBeams = \tiles ->
    shape = Array2D.shape tiles
    dimensions = { x: shape.dimY, y: shape.dimX }
    ys = List.range { start: At 0, end: Before dimensions.y }
    xs = List.range { start: At 0, end: Before dimensions.y }
    leftBeams = ys |> List.map \y -> { location: { x: 0, y }, direction: East }
    rightBeams = ys |> List.map \y -> { location: { x: dimensions.x - 1, y }, direction: West }
    topBeams = xs |> List.map \x -> { location: { x, y: 0 }, direction: South }
    bottomBeams = xs |> List.map \x -> { location: { x, y: dimensions.y - 1 }, direction: North }
    [leftBeams, rightBeams, topBeams, bottomBeams] |> List.join

energizedTilesCount : Set Beam -> Nat
energizedTilesCount = \beams ->
    beams
    |> Set.map \{ location } -> location
    |> Set.len

expect
    result = part2 inputExample1
    result == 51

#
# Utilities
#
# debugTilesWithBeams : Array2D Tile, List Beam -> Array2D Tile
# debugTilesWithBeams = \tiles, beams ->
#     dbg Str.concat "\n" (inspectTilesWithBeams tiles beams)

#     tiles

# inspectTilesWithBeams : Array2D Tile, List Beam -> Str
# inspectTilesWithBeams = \tiles, beams ->
#     mappedTiles =
#         Array2D.map tiles \tile ->
#             when tile is
#                 Empty -> "."
#                 SplitVertical -> "|"
#                 SplitHorizontal -> "-"
#                 DiagonalForward -> "/"
#                 DiagonalBackward -> "\\"
#     beams
#     |> List.walk mappedTiles \accTiles, beam ->
#         directionStr =
#             when beam.direction is
#                 North -> "^"
#                 East -> ">"
#                 South -> "v"
#                 West -> "<"
#         { x, y } = beam.location
#         accTiles |> Array2D.set { x: y, y: x } directionStr
#     |> Array2D.joinWith "" "\n"

# debug = \value ->
#     dbg value

#     value

orCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
