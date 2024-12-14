open System
open System.IO
open MathNet.Numerics.LinearAlgebra

type Point = { X: int64; Y: int64 }
type ClawMachine = { A: Point; B: Point; Prize: Point }

let parseClawMachine (input: string) =
    let lines = input.Split(Environment.NewLine)
    let [| ax; ay |] = lines[0].Substring("Button A: X+".Length).Split(", Y+")
    let [| bx; by |] = lines[1].Substring("Button B: X+".Length).Split(", Y+")
    let [| px; py |] = lines[2].Substring("Prize: X=".Length).Split(", Y=")
    { A = { X = int64 ax; Y = int64 ay }
      B = { X = int64 bx; Y = int64 by }
      Prize = { X = int64 px; Y = int64 py } }

let clawMachines =
    File.ReadAllText("input.txt").Split(Environment.NewLine + Environment.NewLine)
    |> Array.map parseClawMachine

let solveClawMachine machine =
    // A.X * a + B.X * b = P.X
    // A.Y * a + B.Y * b = P.Y
    let A = 
        matrix [
            [ float machine.A.X; float machine.B.X ]
            [ float machine.A.Y; float machine.B.Y ]
        ]
    let b = vector [ float machine.Prize.X; float machine.Prize.Y ]
    let x = A.Solve(b)
    let a = x[0]
    let b = x[1]
    let e = 0.0001
    if a - e < round a && round a < a + e && 
       b - e < round b && round b < b + e
    then Some (int64 (round a), int64 (round b))
    else None

let part1 () =
    clawMachines 
    |> Array.choose solveClawMachine
    |> Array.sumBy (fun (a, b) -> 3L * a + b)

printfn "Part 1: %d" (part1 ())

let part2 () =
    clawMachines
    |> Array.map (fun m -> 
        { m with 
            Prize.X = m.Prize.X + 10000000000000L
            Prize.Y = m.Prize.Y + 10000000000000L })
    |> Array.choose solveClawMachine
    |> Array.sumBy (fun (a, b) -> 3L * a + b)

printfn "Part 2: %d" (part2 ())
