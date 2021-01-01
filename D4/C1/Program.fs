open System
open Utils

let required =
    List.sort [ "byr"
                "iyr"
                "eyr"
                "hgt"
                "hcl"
                "ecl"
                "pid" ]

let ignored = [ "cid" ]

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
            |> Seq.choose2 Boolean.testEquality required
            |> Seq.length
            |> Boolean.testEqualityFor entry required.Length)
    |> Seq.length

    // Output result
    |> printfn " ##### Result %d"

    0
