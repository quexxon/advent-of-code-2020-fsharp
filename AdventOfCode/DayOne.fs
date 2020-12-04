module AdventOfCode.DayOne

open System.IO

let part1 (input: int []) =
    let target = 2020
    Array.sortInPlace input

    let rec loop x y =
        let sum = input.[x] + input.[y]
        if x = y then failwith "Failed to find pair"
        elif sum = target then input.[x] * input.[y]
        elif sum < target then loop (x + 1) y
        else loop x (y - 1)

    loop 0 (input.Length - 1)

let part2 (input: int []) =
    let target = 2020
    Array.sortInPlace input

    let rec outerLoop x =
        let rec innerLoop y z =
            let sum = input.[x] + input.[y] + input.[z]
            if y = z then None
            elif sum = target then Some(input.[x] * input.[y] * input.[z])
            elif sum < target then innerLoop (y + 1) z
            else innerLoop y (z - 1)

        if x = input.Length - 2 then failwith "Failed to find triplet"

        match innerLoop (x + 1) (input.Length - 1) with
        | Some result -> result
        | None -> outerLoop (x + 1)

    outerLoop 0

type Solution() as self =
    inherit Util.Solution<int>("Day One", "01.txt")

    let input =
        File.ReadAllLines self.InputPath |> Array.map int

    override __.Part1() = part1 (Array.copy input)

    override __.Part2() = part2 (Array.copy input)
