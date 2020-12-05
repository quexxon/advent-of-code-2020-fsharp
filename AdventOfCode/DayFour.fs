module AdventOfCode.DayFour

open System.IO
open System.Text.RegularExpressions

type PassportField =
    | BirthYear = 0b1000_0000uy
    | IssueYear = 0b0100_0000uy
    | ExpirationYear = 0b0010_0000uy
    | Height = 0b0001_0000uy
    | HairColor = 0b0000_1000uy
    | EyeColor = 0b0000_0100uy
    | PassportId = 0b0000_0010uy
    | CountryId = 0b0000_0001uy

let toPassportField (str: string): PassportField =
    match str with
    | "byr" -> PassportField.BirthYear
    | "iyr" -> PassportField.IssueYear
    | "eyr" -> PassportField.ExpirationYear
    | "hgt" -> PassportField.Height
    | "hcl" -> PassportField.HairColor
    | "ecl" -> PassportField.EyeColor
    | "pid" -> PassportField.PassportId
    | "cid" -> PassportField.CountryId
    | field -> failwith $"Unknown passport field: {field}"

let isValidPassportField (field: PassportField) (value: string): bool =
    try
        match field with
        | PassportField.BirthYear ->
            let year = int value
            value.Length = 4 && year >= 1920 && year <= 2002
        | PassportField.IssueYear ->
            let year = int value
            value.Length = 4 && year >= 2010 && year <= 2020
        | PassportField.ExpirationYear ->
            let year = int value
            value.Length = 4 && year >= 2020 && year <= 2030
        | PassportField.Height ->
            match Regex.Match(value, @"^(?<height>[1-9]\d+)(?<units>(cm|in))$") with
            | m when m.Success ->
                let height = int m.Groups.["height"].Value
                match m.Groups.["units"].Value with
                | "cm" -> height >= 150 && height <= 193
                | "in" -> height >= 59 && height <= 76
                | _ -> false
            | _ -> false
        | PassportField.HairColor -> Regex.IsMatch(value, @"^#[0-9a-f]{6}$")
        | PassportField.EyeColor ->
            Regex.IsMatch(value, @"^(amb|blu|brn|gry|grn|hzl|oth)$")
        | PassportField.PassportId -> Regex.IsMatch(value, @"^\d{9}$")
        | PassportField.CountryId -> true
        | _ -> false
    with _ -> false

let hasPassportFields (fields: byte): bool = fields >= 0b1111_1110uy

let isValidPassport (passport: string []): bool =
    hasPassportFields
    <| Array.fold (fun fields (field: string) ->
        match field.Split(':') with
        | [| field; _ |] -> fields ||| byte (toPassportField field)
        | _ -> failwith $"Invalid field format: {field}") 0uy passport

let isValidPassport' (passport: string []): bool =
    let (allValid, fields) =
        Array.fold (fun (allValid, fields) (field: string) ->
            match field.Split(':') with
            | [| field; value |] ->
                let field = toPassportField field

                let allValid =
                    allValid && isValidPassportField field value

                (allValid, fields ||| byte field)
            | _ -> failwith $"Invalid field format: {field}") (true, 0uy) passport

    allValid && hasPassportFields fields

let part1 input =
    input
    |> Array.sumBy (fun passport -> if isValidPassport passport then 1 else 0)

let part2 input =
    input
    |> Array.sumBy (fun passport -> if isValidPassport' passport then 1 else 0)

type Solution() as self =
    inherit Util.Solution<int>("Day Four", "04.txt")

    let input =
        File.ReadAllText self.InputPath
        |> (fun s -> s.Split("\n\n"))
        |> Array.map (fun s -> Regex.Replace(s, @"\s+", " ").Trim().Split())

    override __.Part1() = part1 input

    override __.Part2() = part2 input
