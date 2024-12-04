open System.IO

let wordSearch =
    File.ReadAllLines("input.test.txt")
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let isValid (y, x) arr =
    0 <= y && y < Array2D.length1 arr && 
    0 <= x && x < Array2D.length2 arr

let nextPos wordSearch (y, x) =
    [ for dy = -1 to 1 do 
        for dx = -1 to 1 do 
            if not (dy = 0 && dx = 0) && isValid (y + dy, x + dx) wordSearch 
            then (y + dy, x + dx) ]
let rec searchWord (wordSearch: 'char array2d) word positionLists =
    match word with
    | char::word ->
        positionLists 
        |> List.collect (fun ((cY, cX)::positions) ->
            if wordSearch[cY, cX] = char then 
                nextPos wordSearch (cY, cX)
                |> List.map (fun nextPos -> nextPos::(cX, cY)::positions)
            else 
                List.empty
        )
        |> searchWord wordSearch word
    | [] ->
        positionLists
        |> List.map List.tail
        |> List.distinct

let part1 () =
    let allPositions = 
        [ for y = 0 to Array2D.length1 wordSearch - 1 do
            for x = 0 to Array2D.length2 wordSearch - 1 do
                [ y, x ] ]
    searchWord wordSearch (Seq.toList "XMAS") allPositions
    // |> List.length

printfn "Part 1: %A" (part1 ())
