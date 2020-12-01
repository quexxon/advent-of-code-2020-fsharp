module AdventOfCode.Util

open System.Diagnostics
open System.IO

let private measureAndLogResult tag f =
    let stopwatch = Stopwatch.StartNew()
    let result = f ()
    stopwatch.Stop()
    printfn $"{tag}:\nResult: {result}\nElapsed: {stopwatch.Elapsed}\n"

[<AbstractClass>]
type Solution<'a>(name: string, inputFile: string) =
    member internal _.Name = name

    member internal _.InputPath =
        Path.Join(__SOURCE_DIRECTORY__, "input", inputFile)

    abstract Part1: unit -> 'a
    abstract Part2: unit -> 'a

    member internal this.Execute() =
        let line =
            String.replicate (79 - this.Name.Length) "-"

        printfn $"{this.Name.ToUpper()} {line}\n"
        measureAndLogResult "Part 1" this.Part1
        measureAndLogResult "Part 2" this.Part2
