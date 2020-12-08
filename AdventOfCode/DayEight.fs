module AdventOfCode.DayEight

open System.Collections.Generic
open System.IO

type Instruction =
    | Nop of ignore: int
    | Acc of value: int
    | Jmp of offset: int

module Instruction =
    let parse (instruction: string): Instruction =
        match instruction.Trim().Split() with
        | [| opcode; parameter |] ->
            match opcode with
            | "nop" -> Nop(int parameter)
            | "acc" -> Acc(int parameter)
            | "jmp" -> Jmp(int parameter)
            | _ -> failwith $"Unknown opcode: {opcode}"
        | _ -> failwith $"Invalid instruction format: {instruction}"

type StackFrame =
    { Instruction: Instruction
      PC: int
      Acc: int }

type CPU =
    { Memory: Instruction []
      Visited: bool []
      mutable PC: int
      mutable Acc: int }

module CPU =
    let create (program: Instruction []) =
        { Memory = program
          Visited = Array.zeroCreate program.Length
          PC = 0
          Acc = 0 }

    let run (cpu: CPU): Result<int, int> =
        let rec tick () =
            if cpu.PC = cpu.Memory.Length then
                Ok cpu.Acc
            elif cpu.Visited.[cpu.PC] then
                Error cpu.PC
            else
                let instruction = cpu.Memory.[cpu.PC]
                cpu.Visited.[cpu.PC] <- true
                match instruction with
                | Nop _ -> cpu.PC <- cpu.PC + 1
                | Acc value ->
                    cpu.PC <- cpu.PC + 1
                    cpu.Acc <- cpu.Acc + value
                | Jmp offset -> cpu.PC <- cpu.PC + offset
                tick ()

        tick ()

    let runWithStack (cpu: CPU) (stack: Stack<StackFrame>): Result<int, int> =
        let rec tick () =
            if cpu.PC = cpu.Memory.Length then
                Ok cpu.Acc
            elif cpu.Visited.[cpu.PC] then
                Error cpu.PC
            else
                let instruction = cpu.Memory.[cpu.PC]
                cpu.Visited.[cpu.PC] <- true
                stack.Push
                    { Instruction = instruction
                      PC = cpu.PC
                      Acc = cpu.Acc }
                match instruction with
                | Nop _ -> cpu.PC <- cpu.PC + 1
                | Acc value ->
                    cpu.PC <- cpu.PC + 1
                    cpu.Acc <- cpu.Acc + value
                | Jmp offset -> cpu.PC <- cpu.PC + offset
                tick ()

        tick ()


let part1 (program: Instruction []) =
    let cpu = CPU.create program

    match CPU.run cpu with
    | Ok _ -> failwith "No loop found"
    | Error _ -> cpu.Acc

let part2 (program: Instruction []) =
    let cpu = CPU.create program
    let stack = Stack<StackFrame>()
    let mutable result = None

    match CPU.runWithStack cpu stack with
    | Ok _ -> failwith "No loop found"
    | Error _ ->
        while stack.Count > 0 && result = None do
            let frame = stack.Pop()
            cpu.Visited.[frame.PC] <- false

            match frame.Instruction with
            | Nop value -> cpu.Memory.[frame.PC] <- Jmp value
            | Jmp value -> cpu.Memory.[frame.PC] <- Nop value
            | _ -> ()

            if frame.Instruction <> cpu.Memory.[frame.PC] then
                cpu.PC <- frame.PC
                cpu.Acc <- frame.Acc
                let unwindStack = Stack<StackFrame>()
                match CPU.runWithStack cpu unwindStack with
                | Ok acc -> result <- Some acc
                | _ ->
                    cpu.Memory.[frame.PC] <- frame.Instruction
                    while unwindStack.Count > 0 do
                        cpu.Visited.[unwindStack.Pop().PC] <- false

    match result with
    | Some acc -> acc
    | None -> failwith "Failed to fix corruption"

type Solution() as self =
    inherit Util.Solution<int>("Day Eight", "08.txt")

    let input =
        File.ReadAllLines self.InputPath
        |> Array.map Instruction.parse

    override __.Part1() = part1 input

    override __.Part2() = part2 input
