open System
open Utils

let getAccOnLoop (program: list<string * int>) =
    let rec getAccOnLoopRec ix acc (ixsHistory: Set<int>) =
        match program.[ix] with
        | "nop", _ -> ix + 1, acc
        | "jmp", increment -> ix + increment, acc
        | "acc", increment -> ix + 1, acc + increment
        | _ -> failwith ""
        |> function
        | ix', _ when ixsHistory.Contains ix' -> acc
        | ix', acc' -> getAccOnLoopRec ix' acc' (ixsHistory.Add ix')

    getAccOnLoopRec 0 0 Set.empty

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> List.map (
        String.split " "
        >> function
        | op :: num :: _ -> op, int num
        | _ -> failwith ""
    )

    // Do the thing
    |> getAccOnLoop

    // Output result
    |> printfn " ##### Result %d"

    0
