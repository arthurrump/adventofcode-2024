open System
open System.IO

let wordSearch =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let positionExists wordSearch (y, x) =
    0 <= y && y < Array2D.length1 wordSearch &&
    0 <= x && x < Array2D.length2 wordSearch

let directions =
    [ for y = -1 to 1 do 
        for x = -1 to 1 do 
            if not (y = 0 && x = 0) then (y, x) ]

let move (dy, dx) dist (y, x) =
    (y + dy * dist, x + dx * dist)

let wordsFromPosition wordLength (wordSearch: char array2d) (y, x) =
    [ for dir in directions do
        if move dir (wordLength - 1) (y, x) |> positionExists wordSearch then
            [| for d = 0 to wordLength - 1 do 
                let (y, x) = move dir d (y, x)
                wordSearch[y, x] |]
            |> String ]

let part1 () =
    [ for y = 0 to Array2D.length1 wordSearch - 1 do
        for x = 0 to Array2D.length2 wordSearch - 1 do
            (y, x) ]
    |> List.collect (wordsFromPosition 4 wordSearch)
    |> List.filter (fun w -> w = "XMAS")
    |> List.length

printfn "Part 1: %A" (part1 ())
