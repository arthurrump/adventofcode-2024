open System.IO
open System.Text.RegularExpressions

let memory = File.ReadAllText("input.txt")

let mulRegex = Regex( """mul\((\d+),(\d+)\)""")

let part1 () =
    mulRegex.Matches(memory)
    |> Seq.sumBy (fun mul -> 
        int mul.Groups[1].Value * int mul.Groups[2].Value
    )

printfn "Part 1: %A" (part1 ())

let mulDoRegex = Regex("""mul\((\d+),(\d+)\)|do\(\)|don't\(\)""")

let part2 () =
    mulDoRegex.Matches(memory)
    // |> Seq.toList
    |> Seq.fold (fun (result, enabled) op ->
        match op.Value[0..2] with
        | "mul" when enabled ->
            let mul = int op.Groups[1].Value * int op.Groups[2].Value
            (result + mul, enabled)
        | "mul" when not enabled ->
            (result, enabled)
        | "do(" ->
            (result, true)
        | "don" ->
            (result, false)
    ) (0, true)
    |> fst

printfn "Part 2: %A" (part2 ())
