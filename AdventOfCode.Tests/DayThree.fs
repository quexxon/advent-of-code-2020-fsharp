module AdventOfCode.Tests.DayThree

open AdventOfCode.DayThree
open Expecto

let input =
    """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#""".Split('\n')

[<Tests>]
let testParseInput =
    testList
        "parseInput"
        [ test "Small input" {
              let actual =
                  array2D [ [ true; false; false ]
                            [ false; true; false ]
                            [ false; false; true ] ]

              let expected = parseInput [| "#.."; ".#."; "..#" |]

              Expect.equal actual expected "We can parse input as a bool [,]"
          } ]

[<Tests>]
let testTreesOnSlope =
    testList
        "treesOnSlope"
        [ test "Slope 1" {
              let actual, expected =
                  treesOnSlope (parseInput input) slopes.[0], 2

              Expect.equal actual expected "Expect to encounter 2 trees"
          }
          test "Slope 2" {
              let actual, expected =
                  treesOnSlope (parseInput input) slopes.[1], 7

              Expect.equal actual expected "Expect to encounter 7 trees"
          }
          test "Slope 3" {
              let actual, expected =
                  treesOnSlope (parseInput input) slopes.[2], 3

              Expect.equal actual expected "Expect to encounter 3 trees"
          }
          test "Slope 4" {
              let actual, expected =
                  treesOnSlope (parseInput input) slopes.[3], 4

              Expect.equal actual expected "Expect to encounter 4 trees"
          }
          test "Slope 5" {
              let actual, expected =
                  treesOnSlope (parseInput input) slopes.[4], 2

              Expect.equal actual expected "Expect to encounter 2 trees"
          } ]

[<Tests>]
let ``Day 3 - Part 1`` =
    testList
        "AOC - Day 3 - Part 1"
        [ test "Example 1" {
              let actual, expected = part1 (parseInput input), 7UL
              Expect.equal actual expected "Expect to encounter 7 trees"
          } ]

[<Tests>]
let ``Day 3 - Part 2`` =
    testList
        "AOC - Day 3 - Part 2"
        [ test "Example 1" {
              let actual, expected = part2 (parseInput input), 336UL
              Expect.equal actual expected "2 * 7 * 3 * 4 * 2 = 336"
          } ]
