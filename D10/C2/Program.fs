open System
open Utils
open Utils.Functional

let n1 = int64 1
let n0 = int64 0
let n4 = int64 4

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToIntSeq
    |> Seq.toList

    // Do the thing
    |> List.sort
    |> fun l -> 0 :: l |> List.map int64
    |> fun l ->
        l
        |> List.mapi tuple
        |> List.fold
            (fun (hist: list<int64>) (i, jolt) ->
                let next =
                    (seq {
                        2
                        3
                     })
                    |> Seq.sumBy
                        (fun j ->
                            if i >= j && jolt - l.[i - j] < n4 then
                                hist.[j - 2]
                            else
                                n0)
                    |> (+) hist.[2]

                next :: [ hist.[2]; hist.[1] ] |> List.rev)
            [ n0; n0; n1 ]
    |> List.item 2

    // Output result
    |> printfn " ##### Result %d"

    0
