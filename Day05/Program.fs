open System.IO

let input = File.ReadAllLines("input.txt")

let rules =
    input 
    |> Array.takeWhile (fun line -> line.Contains('|'))
    |> Array.map (fun line -> 
        let [| left; right |] = line.Split('|')
        int left, int right
    )

let compare rules a b =
    if Array.contains (a, b) rules
    then -1
    elif Array.contains (b, a) rules
    then 1
    else 0

let updates =
    input 
    |> Array.skipWhile (fun line -> not (line.Contains(',')))
    |> Array.map (fun line ->
        line.Split(',')
        |> Array.map int
    )

let isInOrder (rules: Map<int, int>) comp (update: int array) =
    Array.indexed update
    |> Array.forall (fun (i, page) ->
        match Map.tryFind page rules with
        | Some after ->
            match Array.tryFindIndex (fun p -> p = after) update with
            | Some afterI -> comp i afterI
            | None -> true
        | None -> true 
    )

let part1 () =
    updates 
    |> Array.filter (fun update ->
        update = Array.sortWith (compare rules) update)
    |> Array.sumBy (fun update -> update[Array.length update / 2])

printfn "Part 1: %A" (part1 ())

let part2 () =
    updates
    |> Array.choose (fun update ->
        let sorted = Array.sortWith (compare rules) update
        if update <> sorted
        then Some sorted
        else None)
    |> Array.sumBy (fun update -> update[Array.length update / 2])

printfn "Part 2: %A" (part2 ())
