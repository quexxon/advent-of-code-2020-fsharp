module AdventOfCode.DayThree

open System.IO

let slopes =
    [| {| X = 1; Y = 1 |}
       {| X = 3; Y = 1 |}
       {| X = 5; Y = 1 |}
       {| X = 7; Y = 1 |}
       {| X = 1; Y = 2 |} |]

let parseInput (input: string []): bool [,] =
    input
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (((=) '#')))
    |> array2D

let treesOnSlope (grid: bool [,]) (slope: {| X: int; Y: int |}): int =
    let width = Array2D.length2 grid
    let height = Array2D.length1 grid

    let rec loop x y count =
        if y >= height then
            count
        else
            loop
                ((x + slope.X) % width)
                (y + slope.Y)
                (if grid.[y, x] then count + 1 else count)

    loop 0 0 0

let part1 (input: bool [,]): uint64 =
    treesOnSlope input {| X = 3; Y = 1 |} |> uint64

let part2 (input: bool [,]): uint64 =
    slopes
    |> Array.map (treesOnSlope input)
    |> Array.fold (fun product n -> product * uint64 n) 1UL

type Solution() as self =
    inherit Util.Solution<uint64>("Day Three", "03.txt")

    let input =
        File.ReadAllLines self.InputPath |> parseInput

    override __.Part1() = part1 input

    override __.Part2() = part2 input
