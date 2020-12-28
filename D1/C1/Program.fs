open System
open Utils

let findPair f a =
    Seq.allPairs a a
    |> Seq.find (Seq.fromTuple >> f)
    |> Seq.fromTuple

[<EntryPoint; STAThread>]
let main _ =
    Clipboard.get()
    |> String.linesToIntSeq
    |> findPair (Seq.sum >> (=)2020)
    |> Seq.reduce (*)
    |> string
    |> printfn " ##### Result %s"
    0
