app "advent-2023-roc-day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        Part1,
        Part2,
        "input-full.txt" as inputFull : Str,
    ]
    provides [main] to pf

main =
    _ <- Stdout.line "Part 1: \(Num.toStr (Part1.process inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (Part2.process inputFull))"
