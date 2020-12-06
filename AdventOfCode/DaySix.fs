module AdventOfCode.DaySix

open System.IO
open System.Text.RegularExpressions

let part1 (input: string []) =
    input
    |> Array.sumBy (fun s ->
        Regex.Replace(s, @"\s+", "")
        |> Set.ofSeq
        |> Set.count)

let part2 (input: string []) =
    input
    |> Array.sumBy (fun s ->
        Regex.Split(s, @"\s+")
        |> Array.map Set.ofSeq
        |> Array.reduce Set.intersect
        |> Set.count)

type Solution() as self =
    inherit Util.Solution<int>("Day Six", "06.txt")

    let input =
        File.ReadAllText self.InputPath
        |> (fun s -> s.Split("\n\n") |> Array.map (fun s -> s.Trim()))

    override __.Part1() = part1 input

    override __.Part2() = part2 input
