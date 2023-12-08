app "advent-2023-roc-day08"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        parser.Core.{
            apply,
            const,
            skip,
            maybe,
            oneOf,
            oneOrMore,
            chompUntil,
            sepBy,
        },
        parser.String.{ parseStr, string },
        "input-full.txt" as inputFull : Str,
        "input-sample-1.txt" as inputSample1 : Str,
        "input-sample-2.txt" as inputSample2 : Str,
    ]
    provides [main] to pf

Direction : [Left, Right]

Directions : List Direction

LocationID : U16

Location : List U8

Node : { location : List U8, left : LocationID, right : LocationID }

Nodes : Dict LocationID Node

Map : { directions : Directions, nodes : Nodes }

main =
    # _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

locationIdBase = 'Z' - 'A'

firstLocationId = generateLocationId ['A', 'A', 'A']

lastLocationId = generateLocationId ['Z', 'Z', 'Z']

#
# PART 1
#
part1 : Str -> U64
part1 = \input ->
    input
    |> parseMap
    |> start

expect
    result = part1 inputSample1
    result == 2

expect
    result = part1 inputSample2
    result == 6

parseMap : Str -> Map
parseMap = \input ->
    when parseStr mapParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

expect
    { directions, nodes } = parseMap "LR\n\nA = (B, C)\nB = (D, Z)\n"
    nodeKeys = Dict.keys nodes
    nodeValues = Dict.values nodes
    directions == [Left, Right] && nodeKeys == [0, 1] && nodeValues == [{ location: ['A'], left: 1, right: 2 }, { location: ['B'], left: 3, right: 25 }]

mapParser =
    const (\directions -> \nodesList -> { directions, nodes: convertNodes nodesList })
    |> apply (oneOrMore directionParser)
    |> skip (string "\n\n")
    |> apply (nodeParser |> sepBy (string "\n"))
    |> skip (maybe (string "\n"))

directionParser =
    oneOf [
        const Left |> skip (string "L"),
        const Right |> skip (string "R"),
    ]

nodeParser =
    const (\location -> \left -> \right -> { location, left, right })
    |> apply (chompUntil ' ')
    |> skip (string " = (")
    |> apply (chompUntil ',')
    |> skip (string ", ")
    |> apply (chompUntil ')')
    |> skip (string ")")

convertNodes : List { location : Location, left : Location, right : Location } -> Nodes
convertNodes = \nodesList ->
    nodesList
    |> List.walk (Dict.empty {}) \dict, node ->
        key = generateLocationId node.location
        left = generateLocationId node.left
        right = generateLocationId node.right
        dict |> Dict.insert key { location: node.location, left, right }

generateLocationId : Location -> LocationID
generateLocationId = \chars ->
    chars
    |> List.reverse
    |> List.walkWithIndex 0u16 \sum, char, index ->
        sum + (Num.toU16 (char - 'A')) * (Num.powInt locationIdBase (Num.toU16 index))

start : Map -> U64
start = \map ->
    recursiveStep map map.directions firstLocationId 1u64

recursiveStep : Map, Directions, LocationID, U64 -> U64
recursiveStep = \map, directions, locationId, depth ->
    newLocationId = nextLocationId map directions locationId
    if newLocationId == lastLocationId then
        depth
    else
        newDirections = nextDirections map directions
        recursiveStep map newDirections newLocationId (depth + 1)

nextLocationId : Map, Directions, LocationID -> LocationID
nextLocationId = \map, directions, locationId ->
    { left, right } = map.nodes |> Dict.get locationId |> okOrCrash "Invalid location \(Num.toStr locationId)"
    when directions |> List.first is
        Ok Left -> left
        Ok Right -> right
        Err _ -> crash "No directions remaining"

nextDirections : Map, Directions -> Directions
nextDirections = \map, directions ->
    newDirections = directions |> List.dropFirst 1
    if List.isEmpty newDirections then
        map.directions
    else
        newDirections

okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
