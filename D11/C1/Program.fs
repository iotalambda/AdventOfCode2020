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

let getAdjancent x y l =
    adjacentOffsets
    |> Seq.choose
        (fun o ->
            l
            |> List.tryItem (y + snd o)
            |> Option.bind (List.tryItem (x + fst o)))


let rec applyRulesUntilStabilized l =
    let l' =
        l
        |> List.mapi
            (fun y ->
                List.mapi
                    (fun x ->
                        let adjacents = getAdjancent x y l

                        function
                        | 'L' when adjacents |> Seq.all ((<>) '#') -> '#'
                        | '#' when
                            adjacents
                            |> Seq.sumBy ((=) '#' >> Convert.ToInt32)
                            >= 4 -> 'L'
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
