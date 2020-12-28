open System
open Utils
open Utils.Functional
open Utils.String
open Utils.Pair

let findPair = (both Seq.allPairs) >> (flip Seq.find) |> flip

[<EntryPoint; STAThread>]
let main _ =
    Clipboard.get()
    |> linesToIntSeq
    |> findPair (sum >> (=)2020)
    |> multiply
    |> string
    |> printfn " ##### Result %s"
    0
