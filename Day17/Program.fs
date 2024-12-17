open System
open System.IO

type Computer =
    { Registers: int[]
      InstructionPointer: int
      Instructions: int[]
      Output: int list
      Halted: bool }

let computer =
    let input = File.ReadAllLines("input.txt")
    let readRegister (line: string) =
        line.Substring("Register X: ".Length) |> int
    let instructions =
        input[4].Substring("Program: ".Length).Split(",") 
        |> Array.map int
    { Registers = input[0..2] |> Array.map readRegister
      InstructionPointer = 0
      Instructions = instructions
      Output = []
      Halted = false }

module Computer =
    let getRegister index computer =
        computer.Registers[index]

    let setRegister index value computer =
        { computer with Registers = Array.updateAt index value computer.Registers }

    let advance computer =
        { computer with InstructionPointer = computer.InstructionPointer + 2 }

    let output value computer =
        { computer with Output = value::computer.Output }

    let readOutput computer =
        computer.Output |> List.rev

    let readOperand computer =
        computer.Instructions[computer.InstructionPointer + 1]

    let readComboOperand computer =
        let op = readOperand computer
        if op <= 3 then op
        elif 4 <= op && op <= 6 then computer.Registers[op - 4]
        else failwith "Invalid operand"

    let step computer =
        if computer.InstructionPointer < Array.length computer.Instructions then
            match computer.Instructions[computer.InstructionPointer] with
            | 0 -> // adv
                let num = getRegister 0 computer
                let denom = pown 2 (readComboOperand computer)
                computer |> setRegister 0 (num / denom) |> advance
            | 1 -> // bxl
                let b = getRegister 1 computer
                let op = readOperand computer
                computer |> setRegister 1 (b ^^^ op) |> advance
            | 2 -> // bst
                let op = readComboOperand computer
                computer |> setRegister 1 (op % 8) |> advance
            | 3 -> // jnz
                if getRegister 0 computer = 0 then
                    computer |> advance
                else
                    { computer with InstructionPointer = readOperand computer }
            | 4 -> // bxc
                let b = getRegister 1 computer
                let c = getRegister 2 computer
                computer |> setRegister 1 (b ^^^ c) |> advance
            | 5 -> // out
                computer |> output (readComboOperand computer % 8) |> advance
            | 6 -> // bdv
                let num = getRegister 0 computer
                let denom = pown 2 (readComboOperand computer)
                computer |> setRegister 1 (num / denom) |> advance
            | 7 -> // cdv
                let num = getRegister 0 computer
                let denom = pown 2 (readComboOperand computer)
                computer |> setRegister 2 (num / denom) |> advance
        else
            { computer with Halted = true }

    let rec run computer =
        if computer.Halted
        then computer
        else run (step computer)

let part1 () =
    let output =
        computer |> Computer.run |> Computer.readOutput
    String.Join(",", output)

printfn "Part 1: %A" (part1 ())
