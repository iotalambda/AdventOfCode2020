open System
open Utils
open Utils.Functional

let zero = int64 (0)

let toOr =
    String.replace "X" "0"
    >> fun str -> Convert.ToInt64(str, 2)

let getPermutations (mask: string) =

    let terms =
        mask
        |> Seq.rev
        |> Seq.indexed
        |> Seq.where (snd >> (=) 'X')
        |> Seq.map (fst >> pown (int64 (2)))
        |> Seq.toList

    let result =
        [ 1 .. (pown 2 (terms.Length)) ]
        |> List.map
            (fun p ->
                Convert
                    .ToString(p - 1, 2)
                    .PadLeft(terms.Length, '0')
                |> Seq.zip terms
                |> Seq.where (snd >> (=) '1')
                |> Seq.sumBy fst)

    result

let getMem =
    String.replace "mem[" ""
    >> String.replace "]" ""
    >> int64

let exec program =
    let rec aux or' perms reg =
        function
        | head :: tail ->
            match head with
            | "mask" :: mask :: _ -> aux (toOr mask) (getPermutations mask) reg tail
            | memStr :: valStr :: _ ->
                let mem = getMem memStr
                let val' = int64 (valStr)

                let reg =
                    perms
                    |> Seq.map ((^^^) (mem ||| or'))
                    |> Seq.fold (fun s c -> Map.add c val' s) reg

                aux or' perms reg tail
            | _ -> failwith ""
        | [] -> reg

    aux zero [] Map.empty program

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
