open System
open Utils
open Utils.Functional

let slide right down =
    Seq.choosei (
        flip
        <| fun (line: string) ->
            function
            | 0 -> None
            | i when i % down = 0 ->
                line
                |> Seq.item (right * (i / down) % line.Length)
                |> function
                | '#' -> Some 1
                | _ -> None
            | _ -> None
    )
    >> Seq.sum
    >> int64

[<EntryPoint; STAThread>]
let main _ =

    let hill =
        // Get and wrangle data
        Clipboard.get () |> String.linesToStringList

    // Do the thing
    slide 1 1 hill
    * slide 3 1 hill
    * slide 5 1 hill
    * slide 7 1 hill
    * slide 1 2 hill

    // Output result
    |> printfn " ##### Result %d"

    0
