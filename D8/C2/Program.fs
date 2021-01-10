open System
open Utils

let testAccAtEof program =
    let eof = program |> List.length

    let rec testAccAtEofRec ix acc ixsHistory =
        match program.[ix] with
        | "nop", _ -> ix + 1, acc
        | "jmp", increment -> ix + increment, acc
        | "acc", increment -> ix + 1, acc + increment
        | _ -> failwith ""
        |> function
        | ix', acc' when ix' = eof -> Some acc'
        | ix', _ when ixsHistory |> Set.contains ix' -> None
        | ix', acc' -> testAccAtEofRec ix' acc' (ixsHistory.Add ix')

    testAccAtEofRec 0 0 Set.empty

let getAccForTerminatingVariant program =
    program
    |> Seq.mapi Functional.tuple
    |> Seq.pick
        (fun (ix, (op, num)) ->
            match op with
            | "jmp" -> Some "nop"
            | "nop" -> Some "jmp"
            | _ -> None
            |> Option.bind
                (fun op ->
                    program
                    |> List.replace ix (op, num)
                    |> testAccAtEof))


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
    |> getAccForTerminatingVariant

    // Output result
    |> printfn " ##### Result %d"

    0
