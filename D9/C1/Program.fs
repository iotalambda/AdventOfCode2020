open System
open Utils

let tryPickTerms sum candidates =
    Seq.allPairs candidates candidates
    |> Seq.tryPick
        (function
        | a, b when a + b = sum && a <> b -> Some(a, b)
        | _ -> None)

let pickWithoutTerms premableSize =
    Seq.windowed (premableSize + 1)
    >> Seq.pick
        (fun windowAndLast ->
            let sum = windowAndLast |> Seq.last

            let window = windowAndLast |> Seq.take premableSize

            match tryPickTerms sum window with
            | None -> Some sum
            | Some _ -> None)

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToIntSeq

    // Do the thing
    |> pickWithoutTerms 25

    // Output result
    |> printfn " ##### Result %d"

    0
