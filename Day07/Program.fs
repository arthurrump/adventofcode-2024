open System.IO

let parseLine (line: string) =
    let [| result; values |] = line.Split(": ")
    let values = 
        values.Split(" ") 
        |> Array.map bigint.Parse
        |> Array.toList
    bigint.Parse result, values

let equations =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine

let rec possibleResults1 values =
    let first::rest = values
    List.fold (fun results value ->
        Set.union
            (Set.map (fun res -> res + value) results)
            (Set.map (fun res -> res * value) results))
        (Set.singleton first) rest

let part1 () =
    equations
    |> Array.filter (fun (expected, values) -> 
        possibleResults1 values
        |> Set.contains expected)
    |> Array.sumBy fst

printfn "Part 1: %A" (part1 ())

let rec possibleResults2 expected values =
    let first::rest = values
    List.fold (fun results value ->
        Set.unionMany [
            (Set.map (fun res -> res + value) results)
            (Set.map (fun res -> res * value) results)
            (Set.map (fun res -> bigint.Parse (string res + string value)) results)
        ] |> Set.filter (fun res -> res <= expected))
        (Set.singleton first) rest

let part2 () =
    equations
    |> Array.filter (fun (expected, values) -> 
        possibleResults2 expected values
        |> Set.contains expected)
    |> Array.sumBy fst

printfn "Part 2: %A" (part2 ())
