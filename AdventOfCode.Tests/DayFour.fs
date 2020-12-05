module AdventOfCode.Tests.DayFour

open AdventOfCode.DayFour
open Expecto

let example1 =
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
        .Split(' ')

let example2 =
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929".Split(' ')

let example3 =
    "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm".Split(' ')

let example4 =
    "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in".Split(' ')

let part2Input = """eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"""

let validHairColors =
    [ "#a1b002"
      "#234abc"
      "#deadbe"
      "#000000"
      "#ffffff"
      "#019acf" ]

let validEyeColors =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let invalidEyeColors = [ "fish"; "pickle"; "cat"; ""; "    " ]

let validPassportIds =
    [ "000000000"
      "999999999"
      "000000001"
      "123456789"
      "987654321" ]

let invalidPassportIds =
    [ "00000000"
      "99999999999"
      ""
      "   "
      "fish" ]

[<Tests>]
let ``Test isValidPassportField`` =
    testList "isValidPassportField"
    <| [ test "Birth year less than lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.BirthYear "1919", false

             Expect.equal actual expected "1919 is not a valid birth year"
         }
         test "Birth year greater than upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.BirthYear "2003", false

             Expect.equal actual expected "2003 is not a valid birth year"
         }
         test "Birth year lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.BirthYear "1920", true

             Expect.equal actual expected "1920 is the smallest valid birth year"
         }
         test "Birth year upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.BirthYear "2002", true

             Expect.equal actual expected "2002 is the largest valid birth year"
         }
         test "Birth year in valid range" {
             let actual, expected =
                 isValidPassportField PassportField.BirthYear "1988", true

             Expect.equal
                 actual
                 expected
                 "Any year between 1920 and 2002 is a valid birth year"
         }
         test "Issue year less than lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.IssueYear "2009", false

             Expect.equal actual expected "2009 is not a valid issue year"
         }
         test "Issue year greater than upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.IssueYear "2021", false

             Expect.equal actual expected "2021 is not a valid issue year"
         }
         test "Issue year lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.IssueYear "2010", true

             Expect.equal actual expected "2010 is the smallest valid issue year"
         }
         test "Issue year upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.IssueYear "2020", true

             Expect.equal actual expected "2020 is the largest valid issue year"
         }
         test "Issue year in valid range" {
             let actual, expected =
                 isValidPassportField PassportField.IssueYear "2015", true

             Expect.equal
                 actual
                 expected
                 "Any year between 2010 and 2020 is a valid issue year"
         }
         test "Expiration year less than lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.ExpirationYear "2019", false

             Expect.equal actual expected "2019 is not a valid expiration year"
         }
         test "Expiration year greater than upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.ExpirationYear "2031", false

             Expect.equal actual expected "2031 is not a valid expiration year"
         }
         test "Expiration year lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.ExpirationYear "2020", true

             Expect.equal actual expected "2020 is the smallest valid expiration year"
         }
         test "Expiration year upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.ExpirationYear "2030", true

             Expect.equal actual expected "2030 is the largest valid expiration year"
         }
         test "Expiration year in valid range" {
             let actual, expected =
                 isValidPassportField PassportField.ExpirationYear "2025", true

             Expect.equal
                 actual
                 expected
                 "Any year between 2020 and 2030 is a valid expiration year"
         }
         test "Not a height" {
             let actual, expected =
                 isValidPassportField PassportField.Height "fish", false

             Expect.equal actual expected "'fish' is not a valid height"
         }
         test "Invalid height units" {
             let actual, expected =
                 isValidPassportField PassportField.Height "10bin", false

             Expect.equal actual expected "'bin' is not a valid unit of height"
         }
         test "Height with only units" {
             let actual, expected =
                 isValidPassportField PassportField.Height "in", false

             Expect.equal actual expected "A height with only units is invalid"
         }
         test "Out of range height in inches - too low" {
             let actual, expected =
                 isValidPassportField PassportField.Height "58in", false

             Expect.equal actual expected "58 inches is too low"
         }
         test "Out of range height in inches - too high" {
             let actual, expected =
                 isValidPassportField PassportField.Height "77in", false

             Expect.equal actual expected "77 inches is too high"
         }
         test "Height in inches - lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.Height "59in", true

             Expect.equal actual expected "59 inches is the smallest valid height"
         }
         test "Height in inches - upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.Height "76in", true

             Expect.equal actual expected "76 inches is the largest valid height"
         }
         test "Valid height in inches" {
             let actual, expected =
                 isValidPassportField PassportField.Height "63in", true

             Expect.equal
                 actual
                 expected
                 "Any height in inches between 59 and 76 is valid"
         }
         test "Out of range height in centimeters - too low" {
             let actual, expected =
                 isValidPassportField PassportField.Height "149cm", false

             Expect.equal actual expected "149 centimeters is too low"
         }
         test "Out of range height in centimeters - too high" {
             let actual, expected =
                 isValidPassportField PassportField.Height "194cm", false

             Expect.equal actual expected "194cm centimeters is too high"
         }
         test "Height in centimeters - lower bound" {
             let actual, expected =
                 isValidPassportField PassportField.Height "150cm", true

             Expect.equal actual expected "150 centimeters is the smallest valid height"
         }
         test "Height in centimeters - upper bound" {
             let actual, expected =
                 isValidPassportField PassportField.Height "193cm", true

             Expect.equal actual expected "193 centimeters is the largest valid height"
         }
         test "Valid height in centimeters" {
             let actual, expected =
                 isValidPassportField PassportField.Height "163cm", true

             Expect.equal
                 actual
                 expected
                 "Any height in centimeters between 150 and 193 is valid"
         }
         test "Invalid hair color - missing #" {
             let actual, expected =
                 isValidPassportField PassportField.HairColor "000000", false

             Expect.equal actual expected "Hair color must have leading #"
         }
         test "Invalid hair color - invalid hex digit" {
             let actual, expected =
                 isValidPassportField PassportField.HairColor "#0z0000", false

             Expect.equal actual expected "Hair color must be valid hex value"
         }
         test "Invalid hair color" {
             let actual, expected =
                 isValidPassportField PassportField.HairColor "fish", false

             Expect.equal actual expected "'fish' is not a valid hair color"
         } ]
    @ (validHairColors
       |> List.map (fun hairColor ->
           test $"Valid hair color - {hairColor}" {
               let actual, expected =
                   isValidPassportField PassportField.HairColor hairColor, true

               Expect.equal actual expected $"'{hairColor}' is a valid hair color"
           }))
    @ (validEyeColors
       |> List.map (fun eyeColor ->
           test $"Valid eye color - {eyeColor}" {
               let actual, expected =
                   isValidPassportField PassportField.EyeColor eyeColor, true

               Expect.equal actual expected $"'{eyeColor}' is a valid eye color"
           }))
    @ (invalidEyeColors
       |> List.map (fun eyeColor ->
           test $"Invalid eye color - {eyeColor}" {
               let actual, expected =
                   isValidPassportField PassportField.EyeColor eyeColor, false

               Expect.equal actual expected $"'{eyeColor}' is an invalid eye color"
           }))
    @ (validPassportIds
       |> List.map (fun passportId ->
           test $"Valid passport ID - {passportId}" {
               let actual, expected =
                   isValidPassportField PassportField.PassportId passportId, true

               Expect.equal actual expected $"'{passportId}' is a valid passort ID"
           }))
    @ (invalidPassportIds
       |> List.map (fun passportId ->
           test $"Invalid passport ID - {passportId}" {
               let actual, expected =
                   isValidPassportField PassportField.PassportId passportId, false

               Expect.equal actual expected $"'{passportId}' is an invalid passort ID"
           }))

[<Tests>]
let ``Test hasPassportFields`` =
    testList
        "hasPassportFields"
        [ test "Has all passport fields" {
              let actual, expected = hasPassportFields 0b1111_1111uy, true
              Expect.equal actual expected "True if all fields are present"
          }
          test "Missing only Country ID" {
              let actual, expected = hasPassportFields 0b1111_1110uy, true
              Expect.equal
                  actual
                  expected
                  "True if all fields except Country ID are present"
          }
          test "Missing multiple fields" {
              let actual, expected = hasPassportFields 0b1001_1110uy, false
              Expect.equal
                  actual
                  expected
                  "False if any fields except Country ID are missing"
          }
          test "Missing all fields" {
              let actual, expected = hasPassportFields 0b0000_0000uy, false
              Expect.equal actual expected "False if all fields are missing"
          } ]

[<Tests>]
let ``Test isValidPassport`` =
    testList
        "isValidPassport'"
        [ test "Example 1" {
              let actual, expected = isValidPassport example1, true
              Expect.equal actual expected "Example 1 is valid"
          }
          test "Example 2" {
              let actual, expected = isValidPassport example2, false
              Expect.equal actual expected "Example 2 is invalid"
          }
          test "Example 3" {
              let actual, expected = isValidPassport example3, true
              Expect.equal actual expected "Example 3 is valid"
          }
          test "Example 4" {
              let actual, expected = isValidPassport example4, false
              Expect.equal actual expected "Example 4 is invalid"
          } ]

[<Tests>]
let ``AoC - Day 4 - Part 1`` =
    testList
        "AoC - Day 4 - Part 1"
        [ test "Example" {
              let input =
                  [| example1
                     example2
                     example3
                     example4 |]

              let actual, expected = part1 input, 2
              Expect.equal actual expected "Expect 2 valid passports"
          } ]

[<Tests>]
let ``AoC - Day 4 - Part 2`` =
    testList
        "AoC - Day 4 - Part 2"
        [ test "Example" {
              let input =
                  part2Input.Split("\n\n")
                  |> Array.map (fun s -> s.Split())

              let actual, expected = part2 input, 4
              Expect.equal actual expected "Expect 4 valid passports"
          } ]
