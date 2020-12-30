open System
open Utils


[<EntryPoint; STAThread>]
let main _ =
    Clipboard.get ()
    |> String.linesToStringList
    |> Seq.skip 1
    |> Seq.choosei
        (fun i line ->
            line
            |> Seq.item ((3 * (1 + i)) % line.Length)
            |> function
            | '#' -> Some 1
            | _ -> None)
    |> Seq.sum
    |> printfn " ##### Result %d"

    0
