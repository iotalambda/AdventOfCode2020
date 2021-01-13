open System
open Utils

let getNofDiffs diff =
    Seq.pairwise
    >> Seq.sumBy
        (function
        | a, b when b - a = diff -> 1
        | _ -> 0)

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    let input =
        Clipboard.get () // fsharplint:disable-line Hints
        |> String.linesToIntSeq

    // Do the thing
    let sorted = Seq.sort input

    let chain =
        seq {
            seq { 0 }
            sorted
            seq { Seq.last sorted + 3 }
        }
        |> Seq.collect id

    getNofDiffs 1 chain * getNofDiffs 3 chain
    // Output result
    |> printfn " ##### Result %d"

    0
