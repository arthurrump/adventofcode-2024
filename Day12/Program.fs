open System.IO

let garden =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray())

let gardenPlots =
    [ for y = 0 to Array.length garden - 1 do
        for x = 0 to Array.length garden[y] - 1 do
            (y, x) ]

let neighbours map (y, x) =
    [ if y > 0 then (y - 1, x)
      if y < Array.length map - 1 then (y + 1, x)
      if x > 0 then (y, x - 1)
      if x < Array.length map[0] - 1 then (y, x + 1) ]
    |> List.filter (fun (ny, nx) -> map[ny][nx] = map[y][x])
    |> Set.ofList

let bfs neighbours start =
    let rec walk (visited: Set<'a>) (frontier: Set<'a>) =
        if Set.isEmpty frontier then
            visited
        else
            let next = Set.minElement frontier
            let neighbours = neighbours next
            walk 
                (Set.add next visited)
                (Set.remove next frontier + (neighbours - visited))
    
    walk Set.empty (Set.singleton start)

let regions neighbours nodes =
    let rec partition groups ungrouped =
        if Set.isEmpty ungrouped then
            groups
        else
            let next = Set.minElement ungrouped
            let group = bfs neighbours next
            partition (group::groups) (ungrouped - group)

    partition List.empty (set nodes)

let area region =
    Set.count region

let perimeter neighbours region =
    region |> Seq.sumBy (fun plot -> 4 - Seq.length (neighbours plot))

let part1 () =
    regions (neighbours garden) gardenPlots
    |> List.sumBy (fun region -> area region * perimeter (neighbours garden) region)

printfn "Part 1: %A" (part1 ())
