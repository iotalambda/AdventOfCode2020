open System
open Utils

let getSeatId =
    String.replace "F" "0"
    >> String.replace "B" "1"
    >> String.replace "R" "1"
    >> String.replace "L" "0"
    >> fun s -> Convert.ToInt64(s, 2)

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList

    // Do the thing
    |> Seq.map getSeatId
    |> Seq.max

    // Output result
    |> printfn " ##### Result %d"

    0
