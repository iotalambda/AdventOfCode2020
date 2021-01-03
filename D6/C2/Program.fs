open System
open Utils
open Utils.Functional


let a = int 'a'
let masks = [ 0 .. 25 ] |> List.map (pown 2)

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.split (String.repeat 2 String.newline)
    |> Seq.map String.linesToStringSeq

    // Do the thing
    |> Seq.sumBy (
        Seq.map (
            Seq.map (fun c -> masks.[int c - a])
            >> Seq.reduce (|||)
        )
        >> Seq.reduce (&&&)
        >> fun map ->
            masks
            |> Seq.where ((&&&) map >> (<>) 0)
            |> Seq.length
    )

    // Output result
    |> printfn " ##### Result %d"

    0
