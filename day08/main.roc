app "advent-2023-roc-day08"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task,
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
        "input-sample-3.txt" as inputSample3 : Str,
    ]
    provides [main] to pf

Direction : [Left, Right]

Directions : List Direction

LocationID : U16

Location : List U8

# The node type is just used for part 2
NodeType : [StartNodeType, MiddleNodeType, EndNodeType]

Node : { type : NodeType, left : LocationID, right : LocationID }

Nodes : Dict LocationID Node

Map : { directions : Directions, nodes : Nodes }

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

locationIdBase = 'Z' - 'A'

firstLocationId = parseLocationId ['A', 'A', 'A']

finalLocationId = parseLocationId ['Z', 'Z', 'Z']

#
# PART 1
#
part1 : Str -> U64
part1 = \input ->
    map = parseMap input
    recursiveStep1 map map.directions firstLocationId 0u64

expect
    result = part1 inputSample1
    result == 2

expect
    result = part1 inputSample2
    result == 6

recursiveStep1 : Map, Directions, LocationID, U64 -> U64
recursiveStep1 = \map, directions, locationId, depth ->
    node = map.nodes |> Dict.get locationId |> okOrCrash "Invalid location \(Num.toStr locationId)"
    newLocationId = nextLocationId directions node
    if newLocationId == finalLocationId then
        depth + 1
    else
        newDirections = nextDirections map directions
        recursiveStep1 map newDirections newLocationId (depth + 1)

nextLocationId : Directions, Node -> LocationID
nextLocationId = \directions, node ->
    when directions |> List.first is
        Ok Left -> node.left
        Ok Right -> node.right
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

#
# PART 2
#
part2 : Str -> U64
part2 = \input ->
    map = parseMap input
    startingLocationIds map.nodes
    |> List.map \locationId -> recursiveStep2 map map.directions locationId 0u64
    |> leastCommonMultipleInList

expect
    result = part2 inputSample3
    result == 6

startingLocationIds : Nodes -> List LocationID
startingLocationIds = \nodes ->
    nodes
    |> Dict.keepIf \(_id, node) -> node.type == StartNodeType
    |> Dict.keys

recursiveStep2 : Map, Directions, LocationID, U64 -> U64
recursiveStep2 = \map, directions, locationId, depth ->
    node = map.nodes |> Dict.get locationId |> okOrCrash "Invalid location \(Num.toStr locationId)"
    if node.type == EndNodeType then
        depth
    else
        newLocationId = nextLocationId directions node
        newDirections = nextDirections map directions
        recursiveStep2 map newDirections newLocationId (depth + 1)

leastCommonMultipleInList = \list ->
    List.walk list 1 \lcm, num ->
        leastCommonMultiple lcm num

leastCommonMultiple = \a, b ->
    (a * b) // (greatestCommonDivisor a b)

greatestCommonDivisor = \a, b ->
    if b == 0 then
        a
    else
        greatestCommonDivisor b (a % b)

#
# Parser
#
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
    directions == [Left, Right] && nodeKeys == [0, 1] && nodeValues == [{ type: StartNodeType, left: 1, right: 2 }, { type: MiddleNodeType, left: 3, right: 25 }]

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
        key = parseLocationId node.location
        left = parseLocationId node.left
        right = parseLocationId node.right
        type = parseNodeType node.location
        dict |> Dict.insert key { type, left, right }

parseLocationId : Location -> LocationID
parseLocationId = \chars ->
    chars
    |> List.reverse
    |> List.walkWithIndex 0u16 \sum, char, index ->
        sum + (Num.toU16 (char - 'A')) * (Num.powInt locationIdBase (Num.toU16 index))

parseNodeType : Location -> NodeType
parseNodeType = \chars ->
    when chars |> List.last is
        Ok 'A' -> StartNodeType
        Ok 'Z' -> EndNodeType
        Ok _ -> MiddleNodeType
        Err _ -> crash "Unable to parse node type"
