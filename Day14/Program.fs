open System
open System.IO

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
