module AdventOfCode.Tests.DayFive

open AdventOfCode.DayFive
open Expecto

let exampleInput =
    [ "BBFBBFFRRL", 870
      "BBFBBFFRRR", 871
      "BBFBBFBLLL", 872
      "BBFBBFBLLR", 873
      "BBFBBFBLRR", 875
      "BBFBBFBRLL", 876
      "BBFBBFBRLR", 877
      "BBFBBFBRRL", 878 ]

[<Tests>]
let ``Test calculateSeatId`` =
    testList "calculateSeatId"
    <| List.map (fun (input, expected) ->
        test $"Valid input: {input}" {
            Expect.equal (calculateSeatId input) expected $"{input} = {expected}" })
           exampleInput

[<Tests>]
let ``AoC - Day 5 - Part 1`` =
    testList
        "AoC - Day 5 - Part 1"
        [ test "Example 1" {
              let input =
                  [| "BFFFBBFRRR"
                     "FFFBBBFRRR"
                     "BBFFBBFRLL" |]

              let actual, expected = part1 input, 820
              Expect.equal actual expected "The highest seat number is 820"
          } ]

[<Tests>]
let ``AoC - Day 5 - Part 2`` =
    testList
        "AoC - Day 5 - Part 2"
        [ test "Example 1" {
              let input =
                  exampleInput |> List.map fst |> Array.ofList

              let actual, expected = part2 input, 874
              Expect.equal actual expected "874 is the missing seat number"
          } ]
