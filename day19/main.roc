app "advent-2023-roc-day19"
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
            chompWhile,
            sepBy,
        },
        parser.String.{ parseStr, string, digits, anyCodeunit },
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

Condition : { category : U8, operator : U8, value : Nat }

Action : [Accept, Reject, Route (List U8)]

Rule : [NonConditional Action, Conditional Condition Action]

Part : Dict U8 Nat

PartResult : { part : Part, status : [Accepted, Rejected] }

Workflows : Dict (List U8) (List Rule)

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
    |> parseSystem
    |> processParts
    |> List.keepIf \result -> result.status == Accepted
    |> List.map .part
    |> List.map partValue
    |> List.sum

expect
    result = part1 inputExample1
    result == 19114

processParts = \{ workflows, parts } ->
    parts |> List.map \part -> processPart part ['i', 'n'] workflows

processPart : Part, List U8, Workflows -> PartResult
processPart = \part, workflowId, workflows ->
    workflows
    |> Dict.get workflowId
    |> orCrash "No initial workflow found"
    |> List.walkUntil (Err NotFound) \_result, rule ->
        checkRule part rule
    |> orCrash "No matching rule found"
    |> \action ->
        when action is
            Accept -> { part, status: Accepted }
            Reject -> { part, status: Rejected }
            Route route -> processPart part route workflows

checkRule : Part, Rule -> [Break (Result Action [NotFound]), Continue (Result Action [NotFound])]
checkRule = \part, rule ->
    when rule is
        Conditional condition action ->
            if checkPart part condition then
                Break (Ok action)
            else
                Continue (Err NotFound)

        NonConditional action -> Break (Ok action)

checkPart : Part, Condition -> Bool
checkPart = \part, condition ->
    value = Dict.get part condition.category |> orCrash "No part category found"
    when condition.operator is
        '<' -> value < condition.value
        '>' -> value > condition.value
        _ -> crash "Invalid operator"

partValue : Part -> Nat
partValue = \part ->
    part |> Dict.values |> List.sum

#
# Parser
#
parseSystem : Str -> { workflows : Workflows, parts : List Part }
parseSystem = \input ->
    when parseStr systemParser (Str.trim input) is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

systemParser =
    const (\workflows -> \parts -> { workflows: Dict.fromList workflows, parts })
    |> keep (workflowParser |> sepBy (string "\n"))
    |> skip (string "\n\n")
    |> keep (partParser |> sepBy (string "\n"))

workflowParser =
    const (\name -> \rules -> (name, rules))
    |> keep (chompUntil '{')
    |> skip (string "{")
    |> keep (ruleParser |> sepBy (string ","))
    |> skip (string "}")

expect
    result = parseStr workflowParser "a{A,R}"
    result == Ok (['a'], [NonConditional Accept, NonConditional Reject])

ruleParser =
    oneOf [
        conditionalRuleParser,
        const (\action -> NonConditional action) |> keep actionParser,
    ]

actionParser =
    oneOf [
        const Accept |> skip (string "A"),
        const Reject |> skip (string "R"),
        routeRuleParser,
    ]

conditionalRuleParser =
    const (\condition -> \action -> Conditional condition action)
    |> keep conditionParser
    |> skip (string ":")
    |> keep actionParser

expect
    result = parseStr conditionalRuleParser "a<2006:qkq"
    result == Ok (Conditional { category: 'a', operator: '<', value: 2006 } (Route ['q', 'k', 'q']))

expect
    result = parseStr conditionalRuleParser "b>5:A"
    result == Ok (Conditional { category: 'b', operator: '>', value: 5 } Accept)

conditionParser =
    const (\category -> \operator -> \value -> { category, operator, value })
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> keep digits

routeRuleParser =
    const (\route -> Route route)
    |> keep (chompWhile \char -> char >= 'a' && char <= 'z')

partParser =
    const (\parts -> Dict.fromList parts)
    |> skip (string "{")
    |> keep (attributeParser |> sepBy (string ","))
    |> skip (string "}")

expect
    result = parseStr partParser "{a=123,b=456}"
    result == Ok (Dict.fromList [('a', 123), ('b', 456)])

attributeParser =
    const (\category -> \value -> (category, value))
    |> keep anyCodeunit
    |> skip (string "=")
    |> keep digits

expect
    result = parseStr attributeParser "a=123"
    result == Ok ('a', 123)

#
# Utilities
#
# debug = \value ->
#     dbg value

#     value

orCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
