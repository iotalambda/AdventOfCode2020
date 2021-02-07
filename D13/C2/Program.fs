open System
open Utils
open Utils.ActivePatterns

let zero = int64 0
let one = int64 1

let findFirstSubseq (big, small) (bus, offset) =
    let rec loop =
        function
        | x when (x + small + offset) % bus = zero -> (big * bus, x + small)
        | x -> loop (x + big)

    loop zero


[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> Seq.item 1
    |> String.split ","
    |> List.indexed
    |> List.choose
        (function
        | ix, Int bus -> Some(int64 (bus), int64 (ix))
        | _ -> None)

    // Do the thing
    |> Seq.fold findFirstSubseq (one, zero)

    // Output result
    |> snd
    |> printfn " ##### Result %d"

    0
