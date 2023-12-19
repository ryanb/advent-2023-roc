app "advent-2023-roc-day18"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        # pf.Task,
        parser.Core.{
            const,
            keep,
            skip,
            oneOf,
            chompUntil,
            sepBy,
        },
        parser.String.{ parseStr, string, digits },
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Point : { x : I64, y : I64 }

Direction : [Up, Down, Left, Right]

Step : { direction : Direction, distance : Nat, colorHex : List U8 }

Boundary : { fromX : I64, toX : I64, fromY : I64, toY : I64 }

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
    |> parsePlan
    |> digPlan
    |> fillOutside
    |> countInside

expect
    result = part1 inputExample1
    result == 62

digPlan : List Step -> Set Point
digPlan = \steps ->
    state = { location: { x: 0, y: 0 }, dugPoints: Set.empty {} }
    steps
    |> List.walk state \accState, step ->
        newState = digStep step accState.location
        { newState & dugPoints: Set.union accState.dugPoints newState.dugPoints }
    |> .dugPoints

digStep : Step, Point -> { location : Point, dugPoints : Set Point }
digStep = \step, { x, y } ->
    range = List.range { start: At 0, end: Length step.distance }
    distance = Num.toI64 step.distance
    when step.direction is
        Up ->
            dugPoints = range |> List.map (\i -> { x, y: y + i }) |> Set.fromList
            { dugPoints, location: { x, y: y + distance } }

        Down ->
            dugPoints = range |> List.map (\i -> { x, y: y - i }) |> Set.fromList
            { dugPoints, location: { x, y: y - distance } }

        Left ->
            dugPoints = range |> List.map (\i -> { x: x - i, y }) |> Set.fromList
            { dugPoints, location: { x: x - distance, y } }

        Right ->
            dugPoints = range |> List.map (\i -> { x: x + i, y }) |> Set.fromList
            { dugPoints, location: { x: x + distance, y } }

fillOutside : Set Point -> Set Point
fillOutside = \dugPoints ->
    dugBoundary = calculateBoundary dugPoints
    outsideBoundary = {
        fromX: dugBoundary.fromX - 1,
        toX: dugBoundary.toX + 1,
        fromY: dugBoundary.fromY - 1,
        toY: dugBoundary.toY + 1,
    }
    startingPoint = { x: outsideBoundary.fromX, y: outsideBoundary.fromY }
    filledPoints = recursiveFill {
        nextPoints: Set.single startingPoint,
        filledPoints: dugPoints,
        boundary: outsideBoundary,
    }
    # We subtract the dug points because we included them in the fill
    Set.difference filledPoints dugPoints

# I hard difficulty with this recursive fill since Roc seems to only optimize tail recursion
# so I had to make a set of next points that get applied each iteration
recursiveFill : { nextPoints : Set Point, filledPoints : Set Point, boundary : Boundary } -> Set Point
recursiveFill = \{ nextPoints, filledPoints, boundary } ->
    newFilledPoints = filledPoints |> Set.union nextPoints
    newNextPoints =
        nextPoints
        |> Set.walk (Set.empty {}) \accPoints, { x, y } ->
            neighborPoints = [
                { x: x + 1, y },
                { x: x - 1, y },
                { x, y: y + 1 },
                { x, y: y - 1 },
            ]
            neighborPoints
            |> List.walk accPoints \accPoints2, neighborPoint ->
                if inBoundary boundary neighborPoint && !(Set.contains accPoints neighborPoint) && !(Set.contains newFilledPoints neighborPoint) then
                    accPoints2 |> Set.insert neighborPoint
                else
                    accPoints2
    if Set.isEmpty newNextPoints then
        newFilledPoints
    else
        recursiveFill { nextPoints: newNextPoints, filledPoints: newFilledPoints, boundary }

inBoundary : Boundary, Point -> Bool
inBoundary = \boundary, { x, y } ->
    x >= boundary.fromX && x <= boundary.toX && y >= boundary.fromY && y <= boundary.toY

calculateBoundary : Set Point -> Boundary
calculateBoundary = \points ->
    points
    |> Set.walk { fromX: 0, toX: 0, fromY: 0, toY: 0 } \accBoundary, point -> {
        fromX: if point.x < accBoundary.fromX then point.x else accBoundary.fromX,
        toX: if point.x > accBoundary.toX then point.x else accBoundary.toX,
        fromY: if point.y < accBoundary.fromY then point.y else accBoundary.fromY,
        toY: if point.y > accBoundary.toY then point.y else accBoundary.toY,
    }

countInside : Set Point -> Nat
countInside = \outsidePoints ->
    boundary = calculateBoundary outsidePoints
    boundaryArea = (boundary.toX - boundary.fromX + 1) * (boundary.toY - boundary.fromY + 1)
    (Num.toNat boundaryArea) - (Set.len outsidePoints)

#
# Parser
#
parsePlan : Str -> List Step
parsePlan = \input ->
    when parseStr planParser (Str.trim input) is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

planParser =
    const (\steps -> steps)
    |> keep (stepParser |> sepBy (string "\n"))

stepParser =
    const (\direction -> \distance -> \colorHex -> { direction, distance, colorHex })
    |> keep directionParser
    |> skip (string " ")
    |> keep digits
    |> skip (string " (#")
    |> keep (chompUntil ')')
    |> skip (string ")")

directionParser =
    oneOf [
        const Up |> skip (string "L"),
        const Down |> skip (string "R"),
        const Left |> skip (string "U"),
        const Right |> skip (string "D"),
    ]

#
# Utilities
#
# debug = \value ->
#     dbg value

#     value

# orCrash = \result, error ->
#     when result is
#         Ok value -> value
#         Err _ -> crash error
