open System
open Utils
open Utils.ActivePatterns
open Utils.Functional

let required =
    List.sort [ "byr"
                "iyr"
                "eyr"
                "hgt"
                "hcl"
                "ecl"
                "pid" ]

let ignored = [ "cid" ]

let validEyeColors =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let (|Year|_|) =
    function
    | Regex @"^[0-9]{4}$" _ & source -> Some source
    | _ -> None

let testValidity k v =
    match k, v with
    | "byr", Year _ & Between 1920 2002 _
    | "iyr", Year _ & Between 2010 2020 _
    | "eyr", Year _ & Between 2020 2030 _
    | "hgt",
      (Suffix "cm" (Between 150 193 _)
      | Suffix "in" (Between 59 76 _))
    | "hcl", Regex @"^#[0-9a-f]{3,6}$" _
    | "ecl", In validEyeColors _
    | "pid", Regex @"^[0-9]{9}$" _
    | In ignored _, _ -> Some()
    | _ -> None

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.split (String.repeat 2 String.newline)
    |> Seq.map (
        String.replace String.newline " "
        >> String.split " "
        >> Seq.map (String.split ":" >> List.toTuple)
        >> dict
    )

    // Do the thing
    |> Seq.choose
        (fun entry ->
            entry
            |> Dict.keys
            |> Seq.except ignored
            |> Seq.sort

            // Check all values are there
            |> Seq.map2 Boolean.testEquality required

            // Validate values
            |> Seq.map (Option.bind (fun key -> testValidity key entry.[key]))

            |> Seq.choose id
            |> Seq.length
            |> Boolean.testEqualityFor entry required.Length)
    |> Seq.length

    // Output result
    |> printfn " ##### Result %d"

    0
