open System
open Utils

let adjacentOffsets =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]


let getSeen x y l =
    adjacentOffsets
    |> Seq.choose
        (fun (ox, oy) ->
            let rec seeFurther sx sy =
                List.tryItem (sy + oy) l
                |> Option.bind (List.tryItem (sx + ox))
                |> function
                | Some '.' -> seeFurther (sx + ox) (sy + oy)
                | Some x -> Some x
                | _ -> None

            seeFurther x y)

let rec applyRulesUntilStabilized l =
    let l' =
        l
        |> List.mapi
            (fun y ->
                List.mapi
                    (fun x ->
                        let seen = getSeen x y l

                        function
                        | 'L' when seen |> Seq.all ((<>) '#') -> '#'
                        | '#' when
                            seen |> Seq.sumBy ((=) '#' >> Convert.ToInt32)
                            >= 5 -> 'L'
                        | x -> x))

    match Seq.equalBy ((<||) (Seq.equalBy ((<||) (=)))) l l' with
    | true -> l'
    | _ -> applyRulesUntilStabilized l'

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.charsTo2dList

    // Do the thing
    |> applyRulesUntilStabilized
    |> Seq.sumBy (Seq.sumBy ((=) '#' >> Convert.ToInt32))

    // Output result
    |> printfn " ##### Result %d"

    0
