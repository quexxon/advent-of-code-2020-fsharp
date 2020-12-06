module AdventOfCode.DayFive

open System.IO

let calculateSeatId (code: string) =
    let lastIndex = code.Length - 1
    let mutable id = 0

    for i = 0 to lastIndex do
        let chr = code.[i]
        if chr = 'B' || chr = 'R' then id <- id ||| (1 <<< (lastIndex - i))

    id

let part1 input =
    input
    |> Array.maxBy calculateSeatId
    |> calculateSeatId

let part2 input =
    let seatIds = Array.map calculateSeatId input
    Array.sortInPlace seatIds

    let rec loop i prev =
        if i = seatIds.Length then failwith "Failed to locate seat"

        let current = seatIds.[i]
        if prev + 1 <> current then prev + 1 else loop (i + 1) current

    loop 1 seatIds.[0]

type Solution() as self =
    inherit Util.Solution<int>("Day Five", "05.txt")

    let input = File.ReadAllLines self.InputPath

    override __.Part1() = part1 input

    override __.Part2() = part2 input
