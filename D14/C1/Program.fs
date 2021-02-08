open System
open Utils

let zero = int64 (0)

let toAnd =
    String.replace "X" "1"
    >> fun str -> Convert.ToInt64(str, 2)

let toOr =
    String.replace "X" "0"
    >> fun str -> Convert.ToInt64(str, 2)

let getMem =
    String.replace "mem[" ""
    >> String.replace "]" ""
    >> int64

let exec program =
    let rec loop and' or' reg =
        function
        | head :: tail ->
            match head with
            | "mask" :: mask :: _ -> loop (toAnd mask) (toOr mask) reg tail
            | memStr :: valStr :: _ ->
                let val' = int64 (valStr) ||| or' &&& and'
                let mem = getMem memStr
                let reg = reg |> Map.add mem val'
                loop and' or' reg tail
            | _ -> failwith ""
        | [] -> reg

    loop zero zero Map.empty program

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> List.map (String.split " = ")

    // Do the thing
    |> exec
    |> Map.toSeq
    |> Seq.sumBy snd

    // Output result
    |> printfn " ##### Result %d"

    0
