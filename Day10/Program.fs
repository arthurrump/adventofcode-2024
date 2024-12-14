open System.IO

let topoMap =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> 
        line.ToCharArray()
        |> Array.map (fun ch -> int ch - int '0')
    )

let neighbours topoMap (y, x) =
    [ if y > 0 then (y - 1, x)
      if y < Array.length topoMap - 1 then (y + 1, x)
      if x > 0 then (y, x - 1)
      if x < Array.length topoMap[0] - 1 then (y, x + 1) ]
    |> List.filter (fun (ny, nx) -> topoMap[ny][nx] = topoMap[y][x] + 1)

let isPeak (topoMap: int array array) (y, x) =
    topoMap[y][x] = 9

let trailheads =
    [ for y = 0 to Array.length topoMap - 1 do
        for x = 0 to Array.length topoMap[y] - 1 do
            if topoMap[y][x] = 0 then
                yield (y, x) ]

let dfs (neighbours: 'a -> 'a list) (isDest: 'a -> bool) (start: 'a) =
    let rec walk (current::path: 'a list) : 'a list list =
        [ for n in neighbours current do
            if isDest n 
            then [ n::current::path ]
            else walk (n::current::path) ]
        |> List.concat

    walk [start]

let part1 () =
    trailheads
    |> List.map (fun trailhead ->
        dfs (neighbours topoMap) (isPeak topoMap) trailhead
        |> Seq.map List.head
        |> Set.ofSeq
        |> Set.count
    )
    |> List.sum

printfn "Part 1: %A" (part1 ())
