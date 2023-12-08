app "advent-2023-roc-day08"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        parser.Core.{
            buildPrimitiveParser,
            apply,
            const,
            skip,
            maybe,
            oneOf,
            oneOrMore,
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

Nodes : Dict Str (Str, Str)

Map : { directions : Directions, nodes : Nodes }

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
    { directions, nodes } = parseMap "LR\n\nAAA = (BBB, CCC)\nBBB = (DDD, ZZZ)\n"
    nodeKeys = Dict.keys nodes
    nodeValues = Dict.values nodes
    directions == [Left, Right] && nodeKeys == ["AAA", "BBB"] && nodeValues == [("BBB", "CCC"), ("DDD", "ZZZ")]

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
    |> apply (anyStringUntil ' ')
    |> skip (string " = (")
    |> apply (anyStringUntil ',')
    |> skip (string ", ")
    |> apply (anyStringUntil ')')
    |> skip (string ")")

# Based on parser chompUntil but returns a string instead of list of characters
anyStringUntil = \char ->
    buildPrimitiveParser \input ->
        when List.findFirstIndex input (\x -> Bool.isEq x char) is
            Ok index ->
                sublist = List.sublist input { start: 0, len: index }
                when Str.fromUtf8 sublist is
                    Ok val -> Ok { val, input: List.dropFirst input index }
                    Err _ -> Err (ParsingFailure "invalid char bytes")

            Err _ -> Err (ParsingFailure "character not found")

convertNodes : List { location : Str, left : Str, right : Str } -> Nodes
convertNodes = \nodesList ->
    nodesList
    |> List.walk (Dict.empty {}) \dict, node ->
        dict |> Dict.insert node.location (node.left, node.right)

start : Map -> I64
start = \map ->
    recursiveStep map map.directions "AAA" 1i64

recursiveStep : Map, Directions, Str, I64 -> I64
recursiveStep = \map, directions, location, depth ->
    newLocation = nextLocation map directions location
    if newLocation == "ZZZ" then
        depth
    else
        newDirections = nextDirections map directions
        recursiveStep map newDirections newLocation (depth + 1)

nextLocation : Map, Directions, Str -> Str
nextLocation = \map, directions, location ->
    (left, right) = map.nodes |> Dict.get location |> okOrCrash "Invalid location \(location)"
    when directions |> List.first is
        Ok Left -> left
        Ok Right -> right
        Err _ -> crash "No directions left"

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
