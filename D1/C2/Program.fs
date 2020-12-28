open System
open Utils

let findTriple f a =
    Seq.allPairs a (Seq.allPairs a a)
    |> Seq.map (fun (a, (b, c)) -> (a, b, c))
    |> Seq.find (Seq.fromTuple3 >> f)
    |> Seq.fromTuple3

[<EntryPoint; STAThread>]
let main _ =
    Clipboard.get()
    |> String.linesToIntSeq
    |> findTriple (Seq.sum >> (=)2020)
    |> Seq.reduce (*)
    |> string
    |> printfn " ##### Result %s"
    0