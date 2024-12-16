open System.IO
open System.Collections.Generic

type Direction = North | East | South | West

let map = 
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray())

module Direction =
    let move (y, x) = function
        | North -> (y - 1, x)
        | East -> (y, x - 1)
        | South -> (y + 1, x)
        | West -> (y, x + 1)

    let rotations = function
        | North | South -> set [ East; West ]
        | East | West -> set [ North; South ]

let start map =
    let (y, x) =
        [ for y = 0 to Array.length map - 1 do
            for x = 0 to Array.length map[y] - 1 do
                (y, x) ]
        |> List.find (fun (y, x) -> map[y][x] = 'S')
    (y, x, East)

let neighbours (map: char[][]) (y, x, dir) =
    let directions = 
        Direction.rotations dir
        |> Set.map (fun d -> (y, x, d))
    let (my, mx) = Direction.move (y, x) dir
    if map[my][mx] <> '#'
    then Set.add (my, mx, dir) directions
    else directions

let cost (fy, fx, fd) (ty, tx, td) =
    if fd <> td then 1000 else 1

let isDest (map: char[][]) (y, x, dir) =
    map[y][x] = 'E'

let dijkstra start isDest neighbours cost =
    let unvisited = PriorityQueue<'n, int>()
    unvisited.Enqueue(start, 0)
    let distances = Dictionary<'n, int>()
    distances.Add(start, 0)
    let paths = Dictionary<'n, 'n list>()
    paths.Add(start, [])
    let mutable current = start

    while not (isDest current) do
        current <- unvisited.Dequeue()
        let dist = distances[current]
        for neighbour in neighbours current do
            let dist = dist + cost current neighbour
            match distances.TryGetValue(neighbour) with
            | true, d ->
                if dist < d then
                    distances[neighbour] <- dist
                    paths[neighbour] <- current::paths[current]
                    unvisited.Enqueue(neighbour, dist)
            | false, _ ->
                distances[neighbour] <- dist
                paths[neighbour] <- current::paths[current]
                unvisited.Enqueue(neighbour, dist)
                
    distances[current], List.rev (current::paths[current])

let shortestPathLength =
    dijkstra (start map) (isDest map) (neighbours map) cost
    |> fst

let part1 () = shortestPathLength

printfn "Part 1: %A" (part1 ())

let pathsUnderCost start isDest neighbours cost maxCost =
    let rec walk (found: Set<'a list>) (frontier: Set<int * 'a list>) =
        if Set.isEmpty frontier then
            found
        else
            let pathCost, current::path = Set.minElement frontier
            let frontier = Set.remove (pathCost, current::path) frontier
            
            let viableNeighbours = 
                neighbours current
                |> Set.filter (fun n -> pathCost + cost current n <= maxCost)
            let dests, nexts =
                Set.partition isDest viableNeighbours
            let found =
                Set.map (fun n -> n::current::path) dests
                |> Set.union found
            let frontier =
                Set.map (fun n -> pathCost + cost current n, n::current::path) nexts
                |> Set.union frontier

            walk found frontier
    
    walk Set.empty (Set.singleton (0, [ start ]))

let part2 () =
    pathsUnderCost (start map) (isDest map) (neighbours map) cost shortestPathLength
    |> Seq.map (fun path -> 
        path 
        |> List.map (fun (y, x, _) -> (y, x)) 
        |> Set.ofList
    )
    |> Set.unionMany
    |> Set.count

printfn "Part 2: %A" (part2 ())
