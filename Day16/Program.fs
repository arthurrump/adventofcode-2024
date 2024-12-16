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

let part1 () =
    dijkstra (start map) (isDest map) (neighbours map) cost
    |> fst

printfn "Part 1: %A" (part1 ())

let dijkstraAll start isDest neighbours cost =
    let unvisited = PriorityQueue<'n, int>()
    unvisited.Enqueue(start, 0)
    let distances = Dictionary<'n, int>()
    distances.Add(start, 0)
    let paths = Dictionary<'n, Set<'n list>>()
    paths.Add(start, Set.singleton [])
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
                    paths[neighbour] <- Set.map (fun p -> current::p) paths[current]
                    unvisited.Enqueue(neighbour, dist)
                elif dist = d then
                    let foundPaths = paths.GetValueOrDefault(neighbour, Set.empty)
                    paths[neighbour] <- Set.union (Set.map (fun p -> current::p) paths[current]) foundPaths
                    unvisited.Enqueue(neighbour, dist)
            | false, _ ->
                distances[neighbour] <- dist
                paths[neighbour] <- Set.map (fun p -> current::p) paths[current]
                unvisited.Enqueue(neighbour, dist)
                
    distances[current], paths[current] |> Set.map (fun path -> List.rev (current::path))

let part2 () =
    dijkstraAll (start map) (isDest map) (neighbours map) cost
    |> snd
    |> Seq.map (List.map (fun (y, x, _) -> (y, x)) >> Set.ofList)
    |> Set.unionMany
    |> Set.count

printfn "Part 2: %A" (part2 ())
