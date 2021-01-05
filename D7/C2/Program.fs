open System
open Utils
open Utils.ActivePatterns

let getGraph =
    Seq.collect
        (function
        | co1 :: co2 :: _ :: _ :: (((Int _) :: _) & tail) ->
            let rec getEdges =
                function
                | _ :: ci1 :: ci2 :: _ :: tail -> ((ci1, ci2), (co1, co2)) :: (getEdges tail)
                | _ -> []

            getEdges tail
        | _ -> [])
    >> Seq.groupBy fst
    >> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> Seq.toList)
    >> dict

let getChildNodes startAt graph =
    let rec traverse node visited =
        match graph |> Dict.tryItem node with
        | Some cs ->
            cs
            |> List.except visited
            |> List.fold
                (fun (cs, visited) c ->
                    traverse c (Set.add c visited)
                    |> fun (subcs, visited) -> c :: subcs @ cs, visited)
                ([], visited)
        | None -> [], visited

    traverse startAt Set.empty |> fst

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringSeq
    |> Seq.map (String.split " ")
    |> Seq.toList

    // Do the thing
    |> getGraph
    |> getChildNodes ("shiny", "gold")
    |> Seq.length

    // Output result
    |> printfn " ##### Result %d"

    0
