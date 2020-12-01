module AdventOfCode.DayOne

open System.IO

let part1 input =
    let target = 2020

    let rec loop visited input =
        if Seq.isEmpty input then
            failwith "Failed to find pair"
        else
            let x = Seq.head input
            let y = target - x
            if Set.contains y visited
            then x * y
            else loop (Set.add x visited) (Seq.tail input)

    loop Set.empty input

let part2 input =
    let target = 2020
    let arr = Array.ofSeq input
    Array.sortInPlace arr

    let rec outerLoop x =
        let rec innerLoop y z =
            let sum = arr.[x] + arr.[y] + arr.[z]
            if y = z then None
            elif sum = target then Some(arr.[x] * arr.[y] * arr.[z])
            elif sum < target then innerLoop (y + 1) z
            else innerLoop y (z - 1)

        if x = arr.Length - 2 then failwith "Failed to find triplet"

        match innerLoop (x + 1) (arr.Length - 1) with
        | Some result -> result
        | None -> outerLoop (x + 1)

    outerLoop 0

type Solution() as self =
    inherit Util.Solution<int>("Day One", "01.txt")

    let input =
        File.ReadLines self.InputPath |> Seq.map int

    override __.Part1() = part1 input

    override __.Part2() = part2 input
