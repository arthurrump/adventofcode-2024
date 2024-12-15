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
