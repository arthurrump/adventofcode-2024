open System.IO

let inputs =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> 
        let [| left; right |] = line.Split("   ")
        int left, int right)

let part1 () =
    inputs
    |> Array.unzip
    |> fun (left, right) ->
        (Array.sort left, Array.sort right)
    ||> Array.zip
    |> Array.map (fun (l, r) -> abs (l - r))
    |> Array.sum

// printfn "Part 1: %A" (part1 ())

let part2 () =
    let left, right = Array.unzip inputs
    let rightCounts = 
        Array.countBy id right 
        |> Map.ofArray
    left
    |> Array.sumBy (fun n ->
        let count = 
            Map.tryFind n rightCounts
            |> Option.defaultValue 0
        n * count)

printfn "Part 2: %A" (part2 ())
