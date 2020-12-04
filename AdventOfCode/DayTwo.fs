module AdventOfCode.DayTwo

open System.IO

type Password = Password of string

type PasswordPolicy = { Char: char; Min: int; Max: int }

let parseInputLine (line: string): PasswordPolicy * Password =
    match line.Split(':') with
    | [| policy; password |] ->
        match policy.Split(' ') with
        | [| limits; chr |] ->
            match limits.Split('-') with
            | [| minimum; maximum |] ->
                ({ Char = char chr
                   Min = int minimum
                   Max = int maximum },
                 Password(password.Trim()))
            | _ -> failwith "Invalid limit format"
        | _ -> failwith "Invalid policy format"
    | _ -> failwith "Invalid input format"

let isValidPassword (policy: PasswordPolicy, Password password): bool =
    let rec loop x y occurences =
        if x > y then
            (occurences >= policy.Min)
            && (occurences <= policy.Max)
        elif occurences > policy.Max then
            false
        elif x = y then
            loop
                (x + 1)
                (y - 1)
                (occurences
                 + if password.[x] = policy.Char then 1 else 0)
        else
            loop
                (x + 1)
                (y - 1)
                (occurences
                 + (if password.[x] = policy.Char then 1 else 0)
                 + (if password.[y] = policy.Char then 1 else 0))

    loop 0 (password.Length - 1) 0

let isValidPassword' (policy: PasswordPolicy, Password password): bool =
    (password.[policy.Min - 1] = policy.Char)
    <> (password.[policy.Max - 1] = policy.Char)

let part1 input =
    input
    |> Array.filter isValidPassword
    |> Array.length

let part2 input =
    input
    |> Array.filter isValidPassword'
    |> Array.length

type Solution() as self =
    inherit Util.Solution<int>("Day Two", "02.txt")

    let input =
        File.ReadAllLines self.InputPath
        |> Array.map parseInputLine

    override __.Part1() = part1 input

    override __.Part2() = part2 input
