open System
open Utils

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> function
    | [ time; buses ] ->
        (int time,
         buses
         |> String.split ","
         |> Seq.where ((<>) "x")
         |> Seq.map int)
    | _ -> failwith ""

    // Do the thing
    |> fun (time, buses) ->
        buses
        |> Seq.map (fun id -> (id, id - (time % id)))
        |> Seq.minBy snd
        ||> (*)

    // Output result
    |> printfn " ##### Result %d"

    0
