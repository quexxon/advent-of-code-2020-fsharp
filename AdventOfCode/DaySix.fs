module AdventOfCode.DaySix

open System
open System.IO

type BitSet = int

module BitSet =
    let private offset = int 'a'

    let addAlpha (chr: Char) (set: BitSet): BitSet = set ||| (1 <<< (int chr - offset))

    let ofAlpha (str: string): BitSet =
        let mutable set = 0

        for chr in str do
            set <- addAlpha chr set

        set

    let count (set: BitSet): int =
        let rec loop count set =
            if set = 0 then count else loop (count + (set &&& 1)) (set >>> 1)

        loop 0 set

    let union (set1: BitSet) (set2: BitSet): BitSet = set1 ||| set2

    let intersect (set1: BitSet) (set2: BitSet): BitSet = set1 &&& set2

let part1 (input: string []) =
    let mutable sum = 0

    for str in input do
        let mutable bitset = 0

        for chr in str do
            if Char.IsLetter(chr) then bitset <- BitSet.addAlpha chr bitset

        sum <- sum + BitSet.count bitset

    sum

let part2 (input: string []) =
    let mutable sum = 0

    for str in input do
        let mutable groupSet = Int32.MinValue
        let mutable set = 0

        for chr in str do
            if chr = '\n' then
                if groupSet = Int32.MinValue
                then groupSet <- set
                else groupSet <- BitSet.intersect groupSet set
                set <- 0
            elif Char.IsLetter(chr) then
                set <- BitSet.addAlpha chr set

        if groupSet = Int32.MinValue
        then groupSet <- set
        else groupSet <- BitSet.intersect groupSet set
        sum <- sum + BitSet.count groupSet

    sum

type Solution() as self =
    inherit Util.Solution<int>("Day Six", "06.txt")

    let input =
        File.ReadAllText self.InputPath
        |> (fun s -> s.Split("\n\n") |> Array.map (fun s -> s.Trim()))

    override __.Part1() = part1 input

    override __.Part2() = part2 input
