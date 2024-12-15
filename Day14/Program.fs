open System.IO
open MathNet.Numerics
open MathNet.Numerics.Statistics

let parseLine (line: string) =
    let [| p; v |] = line.Split(" ")
    let [| px; py |] = p.Substring(2).Split(",")
    let [| vx; vy |] = v.Substring(2).Split(",")
    (int px, int py), (int vx, int vy)

let robots =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine

let width = 101
let height = 103

let modulo n m =
    let n = n % m
    if n < 0
    then n + m
    else n

let positionAt seconds ((px, py), (vx, vy)) =
    modulo (px + seconds * vx) width, modulo (py + seconds * vy) height

let part1 () =
    let positions = robots |> Array.map (positionAt 100)
    let top = Array.filter (fun (_, y) -> y < height / 2)
    let bottom = Array.filter (fun (_, y) -> y > height / 2)
    let left = Array.filter (fun (x, _) -> x < width / 2)
    let right = Array.filter (fun (x, _) -> x > width / 2)
    let t = top positions
    let tl = left t |> Array.length
    let tr = right t |> Array.length
    let b = bottom positions
    let bl = left b |> Array.length
    let br = right b |> Array.length
    tl * tr * bl * br

printfn "Part 1: %A" (part1 ())

// TODO https://www.reddit.com/r/adventofcode/comments/1he0asr/2024_day_14_part_2_why_have_fun_with_image/

let printRobots positions =
    let positionCount =
        Seq.countBy id positions
        |> Map.ofSeq
    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            match Map.tryFind (x, y) positionCount with
            | Some n -> printf "%d" n
            | None -> printf "."
        printfn ""

let step robots =
    robots |> Array.map (fun ((px, py), (vx, vy)) ->
        (modulo (px + vx) width, modulo (py + vy) height), (vx, vy)
    )

let part2 () =
    let positions: (int * (int * int) array) list =
        Seq.init 200 id
        |> Seq.mapFold (fun robots i -> 
            let res = step robots
            (i, Array.map fst res), res
        ) robots
        |> fst
        |> Seq.toList

    let bxs = 
        positions 
        |> List.groupBy (snd >> Array.map (fst >> float) >> Statistics.Variance)
        |> List.minBy fst
        |> snd |> List.map fst
    let bys =
        positions
        |> List.groupBy (snd >> Array.map (snd >> float) >> Statistics.Variance)
        |> List.minBy fst
        |> snd |> List.map fst

    let bx = bxs[0]
    let dx = bxs[1] - bxs[0]
    let by = bys[0]
    let dy = bys[1] - bys[0]

    // bx + dx * x = by + dy * y
    // bx + dx * k = by  (mod dy)
    // dx * k = by - bx  (mod dy)
    let k =
        Seq.initInfinite id
        |> Seq.find (fun k -> Euclid.Modulus(dx * k, dy) = Euclid.Modulus(by - bx, dy))
    
    // For some reason t is one smaller than te actual answer...
    let t = bx + dx * k

    for i = t - 2 to t + 2 do
        printfn "%d" i
        robots |> Array.map (positionAt i) |> printRobots
        printfn ""

part2 ()
