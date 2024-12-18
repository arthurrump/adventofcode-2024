open System.IO
open System.Collections.Generic

let fallingBytes =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        let [| x; y |] = line.Split(",")
        int x, int y
    )

let size = 71

let start = (0, 0)
let dest = (size - 1, size - 1)

let isDest pos = pos = dest

let neighbours corrupted (x, y) =
    let neighbours =
        [ if y > 0 then (x, y - 1)
          if x < size - 1 then (x + 1, y)
          if y < size - 1 then (x, y + 1)
          if x > 0 then (x - 1, y) ]
        |> Set.ofList
    neighbours - corrupted

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
    let corrupted = 
        Array.take 1024 fallingBytes
        |> Set.ofArray
    dijkstra start isDest (neighbours corrupted) (fun _ _ -> 1)
    |> fst

// printfn "Part 1: %A" (part1 ())

let corruptNeighbours corrupted (x, y) =
    let neighbours =
        [ if y > 0 then (x, y - 1)
          if y > 0 && y < size - 1 then (x + 1, y - 1)
          if x < size - 1 then (x + 1, y)
          if y < size - 1 && x < size - 1 then (x + 1, y + 1)
          if y < size - 1 then (x, y + 1)
          if y < size - 1 && x > 0 then (x - 1, y + 1)
          if x > 0 then (x - 1, y)
          if y > 0 && x > 0 then (x - 1, y - 1) ]
        |> Set.ofList
    Set.intersect neighbours corrupted

let corruptStarts =
    Set.filter (fun (x, y) -> x = size - 1 || y = 0)

let corruptIsDest (x, y) =
    x = 0 || y = size - 1

let allPaths start isDest neighbours =
    let rec walk (paths: 'n list list) (frontier: 'n list list) =
        match frontier with
        | [] ->
            paths
        | next::frontier ->
            if List.head next |> isDest then
                walk (next::paths) frontier
            else
                let frontier' =
                    neighbours (List.head next)
                    |> Seq.filter (fun n -> not (List.contains n next))
                    |> Seq.map (fun n -> n::next)
                    |> Seq.toList
                walk paths (frontier' @ frontier)

    walk [] [ [ start ] ]

// Find all continuous paths (diagonals are continuous) from topright to
// bottomleft separating the two corners. From each path find the last byte
// fallen and from those find the first.

let part2 () =
    let corrupted = set fallingBytes
    let indices =
        fallingBytes 
        |> Array.indexed 
        |> Array.map (fun (i, pos) -> pos, i)
        |> Map.ofArray
    let index pos = Map.find pos indices
    corruptStarts corrupted
    |> Seq.collect (fun start -> 
        allPaths start corruptIsDest (corruptNeighbours corrupted)
    )
    |> Seq.map (List.maxBy index)
    |> Seq.minBy index

printfn "Part 2: %A" (part2 ())
