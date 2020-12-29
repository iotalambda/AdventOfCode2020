open System
open Utils

let testPolicy (i1, i2) letter (pw: String) =
    match pw.[i1-1] = letter <> (pw.[i2-1] = letter) with
    | true -> Some pw
    | _ -> None

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get() // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> Seq.map (
        String.split " "
        >> fun [policy; letter; password] ->
            policy |> String.split "-" |> List.map int |> List.toTuple,
            letter |> String.removePart ":" |> char,
            password
    )

    // Do the thing
    |> Seq.choose ((<|||)testPolicy)
    |> Seq.length

    // Output result
    |> printfn " ##### Result %d"
    0