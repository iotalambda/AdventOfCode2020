open System
open Utils
open Utils.Functional

let toRad = flip (/) 360. >> (*) (2. * Math.PI)

let navigate =
    Seq.fold
        (fun (x, y, dir) ->
            function
            | 'F', am -> x + (Math.Cos(toRad dir) * am), y + (Math.Sin(toRad dir) * am), dir
            | 'L', am -> x, y, dir + am
            | 'R', am -> x, y, dir - am
            | 'N', am -> x, y + am, dir
            | 'S', am -> x, y - am, dir
            | 'E', am -> x + am, y, dir
            | 'W', am -> x - am, y, dir
            | _ -> failwith "")
        (0., 0., 0.)

let getManhattanDist (a: int) (b: int) = Math.Abs a + Math.Abs b

[<EntryPoint; STAThread>]
let main _ =

    // Get and wrangle data
    Clipboard.get () // fsharplint:disable-line Hints
    |> String.linesToStringList
    |> List.map (
        Seq.toList
        >> function
        | op :: am -> (op, am |> Array.ofList |> String |> float)
        | _ -> failwith ""
    )

    // Do the thing
    |> navigate
    |> fun (x, y, _) -> getManhattanDist (Convert.ToInt32 x) (Convert.ToInt32 y)

    // Output result
    |> printfn " ##### Result %d"

    0
