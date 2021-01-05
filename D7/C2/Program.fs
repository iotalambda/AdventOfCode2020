open System
open Utils
open Utils.ActivePatterns

type Node = string * string
type Edge = { Src: Node; Tgt: Node; Amt: int }

let getGraph =
    Seq.collect
        (function
        | co1 :: co2 :: _ :: _ :: (((Int _) :: _) & tail) ->
            let rec getEdges =
                function
                | (Int amt) :: ci1 :: ci2 :: _ :: tail ->
                    { Src = (co1, co2)
                      Tgt = (ci1, ci2)
                      Amt = amt }
                    :: getEdges tail
                | _ -> []

            getEdges tail
        | _ -> [])
    >> Seq.groupBy (fun e -> e.Src)
    >> Seq.map (fun (k, vs) -> k, vs |> Seq.toList)
    >> dict

let getChildAmount startAt graph =
    let rec traverse node visited =
        match graph |> Dict.tryItem node with
        | Some cs ->
            cs
            |> List.fold
                (fun (total, visited) { Tgt = c; Amt = amt } ->
                    traverse c (Set.add c visited)
                    |> fun (subcs, visited) -> total + (1 + subcs) * amt, visited)
                (0, visited)
        | None -> 0, visited

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
    |> getChildAmount ("shiny", "gold")

    // Output result
    |> printfn " ##### Result %d"

    0
