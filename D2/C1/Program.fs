open System
open Utils

let testPolicy (f, t) letter =
    String.toChars
    >> Seq.where ((=)letter)
    >> Seq.length
    >> Math.testBetween f t

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get() // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> Seq.map (
        String.split " "
        >> fun [policy; letter; password] ->
            policy |> String.split "-" |> List.map int |> List.toTuple,
            letter |> String.removePart ":",
            password
    )

    // Do the thing
    |> Seq.choose ((<|||)testPolicy)
    |> Seq.length

    // Output result
    |> printfn " ##### Result %d"
    0
