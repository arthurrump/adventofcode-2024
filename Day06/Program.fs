﻿open System.IO

let map =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let startPosition, guard =
    seq {
        for y = 0 to Array2D.length1 map - 1 do
            for x = 0 to Array2D.length2 map - 1 do
                if List.contains map[y, x] [ '^'; '>'; 'v'; '<' ]
                then ((y, x), map[y, x])
    } |> Seq.exactlyOne

let parseDirection =
    function
    | '^' -> (-1, 0)
    | '>' -> (0, 1)
    | 'v' -> (1, 0)
    | '<' -> (0, -1)

let startDirection = parseDirection guard

let turn =
    function
    | (-1, 0) -> (0, 1)
    | (0, 1) -> (1, 0)
    | (1, 0) -> (0, -1)
    | (0, -1) -> (-1, 0)

let rec walk (map: char[,]) steps (y, x) (dy, dx) =
    let (ny, nx) = (y + dy, x + dx)
    if ny < 0 || Array2D.length1 map <= ny || nx < 0 || Array2D.length2 map <= nx then
        Set.add (y, x) steps
    elif map[ny, nx] = '#' then
        walk map steps (y, x) (turn (dy, dx))
    else
        walk map (Set.add (y, x) steps) (ny, nx) (dy, dx)

let part1 () =
    walk map Set.empty startPosition startDirection
    |> Set.count

printfn "Part 1: %A" (part1 ())

let rec walksLoop map steps (y, x) (dy, dx) extraObstacle =
    let (ny, nx) = (y + dy, x + dx)
    if ny < 0 || Array2D.length1 map <= ny || nx < 0 || Array2D.length2 map <= nx then
        false
    elif steps |> Set.contains ((ny, nx), (dy, dx)) then
        true
    elif (ny, nx) = extraObstacle ||  map[ny, nx] = '#' then
        walksLoop map (Set.add ((y, x), (dy, dx)) steps) (y, x) (turn (dy, dx)) extraObstacle
    else
        walksLoop map (Set.add ((y, x), (dy, dx)) steps) (ny, nx) (dy, dx) extraObstacle

let part2 () =
    seq {
        for (y, x) in walk map Set.empty startPosition startDirection do
            if map[y, x] = '.' then
                (y, x)
    }
    |> Seq.filter (walksLoop map Set.empty startPosition startDirection)
    |> Seq.length

printfn "Part 2: %A" (part2 ())
