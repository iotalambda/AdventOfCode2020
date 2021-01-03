open System
open Utils
open Utils.Functional

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.split (String.repeat 2 String.newline)
    |> Seq.map String.linesToStringSeq

    // Do the thing
    |> Seq.sumBy (
        let a = int 'a'

        Seq.map (Seq.fold (flip <| fun c -> (|||) (int c - a)) 0)
        >> Seq.reduce (&&&)
    )

    // Output result
    |> printfn " ##### Result %d"

    0
