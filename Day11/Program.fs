open System.IO

let stones =
    File.ReadAllText("input.txt").Trim().Split(" ")
    |> Array.map bigint.Parse
    |> Array.toList

let blinkStone num =
    if num = 0I then
        [ 1I ]
    else
        let strnum = string num
        if (strnum.Length % 2 = 0) = (num >= 0I) then
            let middle = 
                if num >= 0I
                then strnum.Length / 2
                else strnum.Length / 2 + 1
            [ bigint.Parse strnum[0..middle - 1]; bigint.Parse strnum[middle..] ]
        else
            [ num * 2024I ]

let blink =
    List.collect blinkStone

let part1 () =
    Seq.init 25 id 
    |> Seq.fold (fun stones _ -> blink stones) stones
    |> List.length

printfn "Part 1: %A" (part1 ())
