app "advent-2023-roc-day12"
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
            oneOrMore,
            sepBy,
        },
        parser.String.{ parseStr, string, digits },
        "input-full.txt" as inputFull : Str,
        "input-example-1.txt" as inputExample1 : Str,
    ]
    provides [main] to pf

SpringsBinary : U32

Spring : [OperationalSpring, DamagedSpring, UnknownSpring]

Row : { springs : List Spring, counts : List U64 }

main =
    # {} <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    # Stdout.line "Part 2: \(Num.toStr (part2 { input: inputFull }))"
    Stdout.line "Part 1: \(Num.toStr (part1 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    input
    |> parseRows
    |> List.map \row -> row |> validArrangements |> List.len
    |> List.sum

expect
    result = part1 inputExample1
    result == 21

validArrangements : Row -> List SpringsBinary
validArrangements = \row ->
    row.counts
    |> possibleArrangements (Num.toU64 (List.len row.springs))
    |> List.keepIf \arrangement -> isValidArrangement arrangement row.springs

possibleArrangements : List U64, U64 -> List SpringsBinary
possibleArrangements = \counts, length ->
    padding = length - ((List.sum counts) + (Num.toU64 (List.len counts)) - 1)
    recursivePossibleArrangements 0u32 counts padding

expect
    result = possibleArrangements [1, 1] 3
    result == [0b101u32]

expect
    result = possibleArrangements [2] 3
    result == [0b110u32, 0b011u32]

recursivePossibleArrangements : SpringsBinary, List U64, U64 -> List SpringsBinary
recursivePossibleArrangements = \binary, counts, padding ->
    if List.len counts > 0 then
        count = List.first counts |> okOrCrash "Invalid counts"
        countBinary = applyGroupCount binary count
        paddedCountBinary =
            if List.len counts > 1 then
                Num.shiftLeftBy countBinary 1
            else
                countBinary
        countBinaries = recursivePossibleArrangements paddedCountBinary (List.dropFirst counts 1) padding
        if padding > 0 then
            paddedBinaries = recursivePossibleArrangements (Num.shiftLeftBy binary 1) counts (padding - 1)
            List.concat countBinaries paddedBinaries
        else
            countBinaries
    else
        [Num.shiftLeftBy binary (Num.toU8 padding)]

applyGroupCount : SpringsBinary, U64 -> SpringsBinary
applyGroupCount = \binary, count ->
    if count > 0 then
        applyGroupCount ((Num.shiftLeftBy binary 1) + 1) (count - 1)
    else
        binary

expect
    result = applyGroupCount 0b10 3
    result == 0b10111u32

isValidArrangement : SpringsBinary, List Spring -> Bool
isValidArrangement = \arrangement, springs ->
    springsBinary = springsToBinary springs
    binaryMask = springsToBinaryMask springs
    Num.bitwiseAnd binaryMask springsBinary == Num.bitwiseAnd binaryMask arrangement

expect
    springs = [DamagedSpring, UnknownSpring, UnknownSpring, OperationalSpring]
    result = isValidArrangement 0b1100u32 springs
    result == Bool.true

expect
    springs = [DamagedSpring, UnknownSpring, UnknownSpring, OperationalSpring]
    result = isValidArrangement 0b1001u32 springs
    result == Bool.false

springsToBinary : List Spring -> SpringsBinary
springsToBinary = \springs ->
    springs
    |> List.walk 0u32 \binary, spring ->
        when spring is
            DamagedSpring -> (Num.shiftLeftBy binary 1) + 1
            OperationalSpring -> Num.shiftLeftBy binary 1
            UnknownSpring -> Num.shiftLeftBy binary 1
expect
    result = springsToBinary [DamagedSpring, OperationalSpring, UnknownSpring, DamagedSpring]
    result == 0b1001u32

springsToBinaryMask : List Spring -> SpringsBinary
springsToBinaryMask = \springs ->
    springs
    |> List.walk 0u32 \binary, spring ->
        when spring is
            DamagedSpring -> (Num.shiftLeftBy binary 1) + 1
            OperationalSpring -> (Num.shiftLeftBy binary 1) + 1
            UnknownSpring -> Num.shiftLeftBy binary 1
expect
    result = springsToBinaryMask [DamagedSpring, OperationalSpring, UnknownSpring, DamagedSpring]
    result == 0b1101u32

#
# Parser
#
parseRows : Str -> List Row
parseRows = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map parseRow

parseRow : Str -> Row
parseRow = \input ->
    when parseStr rowParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

expect
    result = parseRow ".#?? 1,1"
    springs = [OperationalSpring, DamagedSpring, UnknownSpring, UnknownSpring]
    result == { springs, counts: [1, 1] }

rowParser =
    const (\springs -> \counts -> { springs, counts: counts |> List.map Num.toU64 })
    |> keep (oneOrMore springParser)
    |> skip (string " ")
    |> keep (digits |> sepBy (string ","))

springParser =
    oneOf [
        const OperationalSpring |> skip (string "."),
        const DamagedSpring |> skip (string "#"),
        const UnknownSpring |> skip (string "?"),
    ]

#
# Utilities
#
okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error
