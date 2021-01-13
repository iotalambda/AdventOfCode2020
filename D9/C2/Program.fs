open System
open Utils
open Utils.Functional

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

let pickContiguous xmas invalid =
    let rec tryPickContiguous xmas' =
        xmas'
        |> Seq.mapi tuple
        |> Seq.scan (fun (sum, _) (i', a) -> sum + a, i') (0, 0)
        |> Seq.takeWhile (fst >> (>=) invalid)
        |> Seq.last
        |> function
        | sum, i when sum = invalid -> Some(xmas' |> Seq.take i)
        | _ -> tryPickContiguous (Seq.tail xmas')

    tryPickContiguous xmas |> Option.get

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToIntSeq

    // Do the thing
    |> (fun xmas ->
        pickWithoutTerms 25 xmas
        |> pickContiguous xmas
        |> fun range -> (range |> Seq.min) + (range |> Seq.max))

    // Output result
    |> printfn " ##### Result %A"

    0
