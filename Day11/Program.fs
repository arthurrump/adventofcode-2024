open System.IO

let stones =
    File.ReadAllText("input.txt").Trim().Split(" ")
    |> Array.map int64
    |> Array.toList

let blinkStone num =
    if num = 0L then
        [ 1L ]
    else
        let strnum = string num
        if (strnum.Length % 2 = 0) = (num >= 0L) then
            let middle = 
                if num >= 0L
                then strnum.Length / 2
                else strnum.Length / 2 + 1
            [ int64 strnum[0..middle - 1]; int64 strnum[middle..] ]
        else
            [ num * 2024L ]

let blink =
    List.collect blinkStone

let part1 () =
    Seq.init 25 id 
    |> Seq.fold (fun stones _ -> blink stones) stones
    |> List.length

printfn "Part 1: %A" (part1 ())

let optimize stones =
    stones
    |> List.groupBy id
    |> List.map (fun (num, lst) -> num, int64 (List.length lst))

let blinkOptimized (optiStones: (int64 * int64) list) =
    optiStones
    |> List.collect (fun (num, count) ->
        blinkStone num 
        |> List.map (fun n -> n, count)
    )
    |> List.groupBy fst
    |> List.map (fun (num, lst) -> num, List.sumBy snd lst)

let part2 () =
    Seq.init 75 id
    |> Seq.fold (fun stones i -> blinkOptimized stones) (optimize stones)
    |> List.sumBy snd

printfn "Part 2: %d" (part2 ())
