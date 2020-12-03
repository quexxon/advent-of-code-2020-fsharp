module AdventOfCode.Tests.DayOne

open AdventOfCode.DayOne
open Expecto

[<Tests>]
let ``Day 1 - Part 1`` =
    testList
        "AOC - Day 1 - Part 1"
        [ test "Example 1" {
              let input = [| 1721; 979; 366; 299; 675; 1456 |]
              let actual, expected = part1 input, 514579
              Expect.equal actual expected "1721 * 299 = 514579"
          } ]

[<Tests>]
let ``Day 1 - Part 2`` =
    testList
        "AOC - Day 1 - Part 2"
        [ test "Example 1" {
              let input = [| 1721; 979; 366; 299; 675; 1456 |]
              let actual, expected = part2 input, 241861950
              Expect.equal actual expected "979 * 366 * 675 = 241861950"
          } ]
