interface Part2
    exposes [process]
    imports ["input-sample-2.txt" as inputSample : Str]

numberWords = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]

numberElements = List.mapWithIndex numberWords \word, index ->
    num = index + 1
    {num, digit: [Num.toStr num], letters: Str.graphemes word}

process = \input ->
    input
    |> Str.split "\n"
    |> List.map buildNumber
    |> List.sum

buildNumber = \str ->
    list = Str.graphemes str
    first = firstNumber list |> Num.toStr
    last = lastNumber list |> Num.toStr
    Str.concat first last |> Str.toI32 |> Result.withDefault 0

firstNumber = \list ->
    if list == [] then
        0
    else
        when beginningNumberElement list is
            Ok element -> element.num
            Err NotFound -> list |> List.dropFirst 1 |> firstNumber

beginningNumberElement = \list ->
    List.findFirst numberElements \element ->
        List.startsWith list element.digit || List.startsWith list element.letters

lastNumber = \list ->
    if list == [] then
        0
    else
        when endingNumberElement list is
            Ok element -> element.num
            Err NotFound -> list |> List.dropLast 1 |> lastNumber

endingNumberElement = \list ->
    List.findFirst numberElements \element ->
        List.endsWith list element.digit || List.endsWith list element.letters

expect
    result = process inputSample
    result == 281
