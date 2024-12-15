open System.IO

let input = File.ReadAllLines("input.txt")

type Map = { Robot: int * int; Boxes: Set<int * int>; Walls: Set<int * int> }

let map =
    let map = 
        input
        |> Array.takeWhile (fun line -> line <> "")
        |> Array.map (fun line -> line.ToCharArray())
    let robot, boxes, walls =
        [ for y = 0 to Array.length map - 1 do
            for x = 0 to Array.length map[y] - 1 do
                (y, x) ]
        |> List.fold (fun (robot, boxes, walls) (y, x) -> 
            match map[y][x] with
            | '.' -> robot, boxes, walls
            | '#' -> robot, boxes, Set.add (y, x) walls
            | 'O' -> robot, Set.add (y, x) boxes, walls
            | '@' -> (y, x), boxes, walls
        ) ((-1, -1), Set.empty, Set.empty)
    { Robot = robot; Boxes = boxes; Walls = walls }

let moves =
    input
    |> Array.skipWhile (fun line -> line <> "")
    |> Array.collect (fun line -> line.ToCharArray())
    |> Array.map (function '^' -> (-1, 0) | '>' -> (0, 1) | 'v' -> (1, 0) | '<' -> (0, -1))

let move (dy, dx) (y, x) =
    (y + dy, x + dx)

let left = move (0, -1)
let right = move (0, 1)

let moveRobot map dir =
    let rec mkMove initial pos dir =
        let next = move dir pos
        if map.Walls |> Set.contains next then
            map
        elif map.Boxes |> Set.contains next then
            mkMove false next dir
        else
            if initial then
                { map with Robot = next }
            else
                let nextRobot = move dir map.Robot
                { map with
                    Robot = nextRobot
                    Boxes = map.Boxes |> Set.remove nextRobot |> Set.add next }
    mkMove true map.Robot dir

let part1 () =
    let movedMap = moves |> Array.fold moveRobot map
    movedMap.Boxes |> Seq.sumBy (fun (y, x) -> 100 * y + x)

printfn "Part 1: %A" (part1 ())

let widen map =
    let doubleX (y, x) = y, x * 2
    { Robot = doubleX map.Robot
      Boxes = map.Boxes |> Set.map doubleX
      Walls = map.Walls |> Set.map doubleX }

let push pos dir colSet =
    let nextPos = move dir pos
    match dir with
    | (0, -1) ->
        assert not (colSet |> Set.contains nextPos)
        if colSet |> Set.contains (left nextPos)
        then Set.singleton (left nextPos), Set.singleton (left nextPos)
        else Set.empty, Set.empty
    | (0, 1) ->
        if colSet |> Set.contains nextPos
        then Set.singleton nextPos, Set.singleton (right nextPos)
        else Set.empty, Set.empty
    | (_, 0) ->
        if colSet |> Set.contains nextPos
        then Set.singleton nextPos, set [ nextPos; right nextPos ]
        elif colSet |> Set.contains (left nextPos)
        then Set.singleton (left nextPos), set [ left nextPos; nextPos ]
        else Set.empty, Set.empty

let widenedMoveRobot map dir =
    let rec mkMove pushed frontier dir =
        if Set.isEmpty frontier then
            let moved = pushed |> Set.map (move dir)
            { map with 
                Robot = move dir map.Robot
                Boxes = map.Boxes - pushed + moved }
        else
            let next = Set.minElement frontier
            let frontier = Set.remove next frontier
            if not (Set.isEmpty (fst (push next dir map.Walls))) then
                map
            else
                let pushed', frontier' = push next dir map.Boxes
                mkMove (pushed + pushed') (frontier + frontier') dir
    mkMove Set.empty (Set.singleton map.Robot) dir

let part2 () =
    let movedMap = moves |> Array.fold widenedMoveRobot (widen map)
    movedMap.Boxes |> Seq.sumBy (fun (y, x) -> 100 * y + x)

printfn "Part 2: %A" (part2 ())
