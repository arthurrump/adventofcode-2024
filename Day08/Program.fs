open System.IO

let map =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray())

let inMap (y, x) =
    0 <= y && y < Array.length map &&
    0 <= x && x < Array.length map[0]

let antennas =
    [ for y, row in Array.indexed map do
        for x, cell in Array.indexed row do
            if cell <> '.' then
                cell, (y, x) ]
    |> List.groupBy fst
    |> List.map (fun (ch, lst) -> ch, List.map snd lst)

let antinodePair (y1, x1) (y2, x2) =
    let dy = y1 - y2
    let dx = x1 - x2
    set [ (y1 + dy, x1 + dx); (y2 - dy, x2 - dx) ]

let antinodes antennas =
    List.allPairs antennas antennas
    |> List.filter (fun (a1, a2) -> a1 <> a2)
    |> List.map (fun (a1, a2) -> antinodePair a1 a2)
    |> Set.unionMany

let part1 () =
    antennas
    |> List.map (fun (ch, antennas) ->  antinodes antennas)
    |> Set.unionMany
    |> Set.filter inMap
    |> Set.count

printfn "Part 1: %A" (part1 ())
