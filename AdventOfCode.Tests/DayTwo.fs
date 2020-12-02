module AdventOfCode.Tests.DayTwo

open AdventOfCode.DayTwo
open Expecto

let input =
    [ ({ Min = 1; Max = 3; Char = 'a' }, Password "abcde")
      ({ Min = 1; Max = 3; Char = 'b' }, Password "cdefg")
      ({ Min = 2; Max = 9; Char = 'c' }, Password "ccccccccc") ]

[<Tests>]
let ``Test parseInputLine`` =
    testList
        "parseInputLine"
        [ test "Simple valid input" {
              let input = "1-3 a: abcde"
              let policy = { Min = 1; Max = 3; Char = 'a' }

              let actual, expected =
                  parseInputLine input, (policy, Password "abcde")

              Expect.equal actual expected "We can parse simple, valid input"
          }
          test "Complex valid input" {
              let input = "11-16 r: rrrrbxdrrqrrsrrb"
              let policy = { Min = 11; Max = 16; Char = 'r' }

              let actual, expected =
                  parseInputLine input, (policy, Password "rrrrbxdrrqrrsrrb")

              Expect.equal actual expected "Slightly more complex can be parsed"
          }
          test "Invalid input format" {
              let input = "invalid"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw on invalid input format"
          }
          test "Invalid policy format" {
              let input = "bad_policy: password"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw on invalid policy format"
          }
          test "Invalid limit format" {
              let input = "bad_limit c: password"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw on invalid limit format"
          }
          test "Invalid min limit" {
              let input = "a-10 c: password"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw if the min limit can't be parsed as an int"
          }
          test "Invalid max limit" {
              let input = "1-z c: password"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw if the max limit can't be parsed as an int"
          }
          test "Invalid policy char" {
              let input = "1-2 not_a_char: password"
              Expect.throws (fun () -> parseInputLine input |> ignore)
                  "Throw if the policy char is not a char"
          } ]

[<Tests>]
let ``Test isValidPassword`` =
    testList
        "isValidPassword"
        [ test "Example #1" {
              let policy = { Min = 1; Max = 3; Char = 'a' }
              let password = Password "abcde"

              Expect.isTrue
                  (isValidPassword (policy, password))
                  "Valid: 'a' occurs between 1 and 3 times"
          }
          test "Example #2" {
              let policy = { Min = 1; Max = 3; Char = 'b' }
              let password = Password "cdefg"

              Expect.isFalse
                  (isValidPassword (policy, password))
                  "Invalid: 'b' does not occur between 1 and 3 times"
          }
          test "Example #3" {
              let policy = { Min = 2; Max = 9; Char = 'c' }
              let password = Password "ccccccccc"

              Expect.isTrue
                  (isValidPassword (policy, password))
                  "Valid: 'c' occurs between 2 and 9 times"
          }
          test "Problematic example #1" {
              // Password has an odd number of characters with the deciding
              // occurrence in the middle position.
              let policy = { Min = 2; Max = 3; Char = 'd' }
              let password = Password "dcddd"

              Expect.isFalse
                  (isValidPassword (policy, password))
                  "Invalid: 'd' occurs more than 3 times"
          } ]

[<Tests>]
let ``Test isValidPassword'`` =
    testList
        "isValidPassword'"
        [ test "Example #1" {
              let policy = { Min = 1; Max = 3; Char = 'a' }
              let password = Password "abcde"

              Expect.isTrue
                  (isValidPassword' (policy, password))
                  "Valid: 'a' occurs between 1 and 3 times"
          }
          test "Example #2" {
              let policy = { Min = 1; Max = 3; Char = 'b' }
              let password = Password "cdefg"

              Expect.isFalse
                  (isValidPassword' (policy, password))
                  "Invalid: 'b' does not occur between 1 and 3 times"
          }
          test "Example #3" {
              let policy = { Min = 2; Max = 9; Char = 'c' }
              let password = Password "ccccccccc"

              Expect.isFalse
                  (isValidPassword' (policy, password))
                  "Valid: 'c' occurs between 2 and 9 times"
          } ]

[<Tests>]
let ``Day 2 - Part 1`` =
    testList
        "AOC - Day 2 - Part 1"
        [ test "Example #1" {
              let actual, expected = part1 input, 2

              Expect.equal actual expected "The first and third passwords are valid"
          } ]

[<Tests>]
let ``Day 2 - Part 2`` =
    testList
        "AOC - Day 2 - Part 2"
        [ test "Example #1" {
              let actual, expected = part2 input, 1

              Expect.equal actual expected "Only the first password is valid"
          } ]
