open System.IO

let parseLine (line: string) =
    line.Split(" ")
    |> Array.map int

let data =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine

let isSafe report =
    let pairs = Array.pairwise report
    let increasing = Array.forall (fun (a, b) -> a < b) pairs
    let decreasing = Array.forall (fun (a, b) -> a > b) pairs
    let properDist = Array.forall (fun (a, b) -> abs (a - b) <= 3) pairs
    (increasing || decreasing) && properDist

let part1 () =
    data |> Array.filter isSafe |> Array.length

printfn "Part 1: %A" (part1 ())
