module AdventOfCode.DaySeven

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

module Digraph =
    type T<'a> =
        { Graph: Dictionary<'a, List<string * 'a>>
          mutable Edges: uint64 }

    let create () = { Graph = Dictionary(); Edges = 0UL }

    let addEdge (v: 'a) (w: string * 'a) (dg: T<'a>) =
        if dg.Graph.ContainsKey(v) then
            dg.Graph.[v].Add(w)
        else
            dg.Graph.Add(v, List([ w ]))
        dg.Edges <- dg.Edges + 1UL

    let adjacent (v: 'a) (dg: T<'a>): List<string * 'a> option =
        if dg.Graph.ContainsKey(v) then Some dg.Graph.[v] else None

    let dfs v dg =
        let marked = HashSet<string * 'a>()
        let mutable total = 0

        let rec loop id vertex =
            if vertex = v || marked.Add(id, vertex) then
                match adjacent vertex dg with
                | Some edges ->
                    total <- total + edges.Count
                    for (subId, vertex) in edges do
                        loop (id + subId) vertex
                | None -> ()

        loop "0" v
        (marked, total)

let contentsPattern = @"(\d+) ([\w\s]+) bag"

type InnerBag = { Bag: string; Quantity: int }

type Bag = { Bag: string; Contents: InnerBag [] }

let parseContents (input: string) =
    if input = "no other bags." then
        [||]
    else
        input.Split(',')
        |> Array.map (fun s ->
            let m = Regex.Match(s, contentsPattern)
            if not m.Success then
                failwith $"Failed to parse contents: {input}"
            else
                { Quantity = int m.Groups.[1].Value
                  Bag = m.Groups.[2].Value })

let parseInputLine (line: string) =
    match line.Split(" bags contain ") with
    | [| bag; contents |] ->
        { Bag = bag
          Contents = parseContents contents }
    | _ -> failwith $"Failed to parse input line: {line}"

let part1 (bags: Bag []): int =
    let graph = Digraph.create ()

    for outerBag in bags do
        for innerBag in outerBag.Contents do
            Digraph.addEdge innerBag.Bag ("", outerBag.Bag) graph

    let (connected, _) = Digraph.dfs "shiny gold" graph

    connected.Count

let part2 (bags: Bag []): int =
    let graph = Digraph.create ()
    let id = ref 0

    for outerBag in bags do
        for innerBag in outerBag.Contents do
            for i = 1 to innerBag.Quantity do
                Digraph.addEdge outerBag.Bag (string !id, innerBag.Bag) graph
                incr id

    let (_, count) = Digraph.dfs "shiny gold" graph

    count

type Solution() as self =
    inherit Util.Solution<int>("Day Seven", "07.txt")

    let input =
        File.ReadAllLines self.InputPath
        |> Array.map parseInputLine

    override __.Part1() = part1 input

    override __.Part2() = part2 input
