module AdventOfCode.DayFive

open System.IO

let calculateSeatId (code: string) =
    let rows = 128
    let cols = 8

    let rec loop i rows cols rlb rub clb cub =
        if i = code.Length then
            rlb * 8 + clb
        else
            match code.[i] with
            | 'B' ->
                let rows = rows >>> 1
                loop (i + 1) rows cols (rlb + rows) rub clb cub
            | 'F' ->
                let rows = rows >>> 1
                loop (i + 1) rows cols rlb (rub - rows) clb cub
            | 'R' ->
                let cols = cols >>> 1
                loop (i + 1) rows cols rlb rub (clb + cols) cub
            | 'L' ->
                let cols = cols >>> 1
                loop (i + 1) rows cols rlb rub clb (cub - cols)
            | chr -> failwith $"Unexpected character: {chr}"

    loop 0 rows cols 0 (rows - 1) 0 (cols - 1)

let part1 input =
    let seatIds = Array.map calculateSeatId input
    Array.sortInPlace seatIds
    Array.last seatIds

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
