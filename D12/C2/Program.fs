open System
open Utils
open Utils.Functional

let dist (x1, y1) (x2, y2) =
    Math.Sqrt(Math.Pow(x2 - x1, 2.) + Math.Pow(y2 - y1, 2.))

let angle (x1, y1) (x2, y2) = Math.Atan2(y2 - y1, x2 - x1)

let toRad = flip (/) 360. >> (*) (2. * Math.PI)

let navigate =
    Seq.fold
        (fun (((sx, sy) & ship), ((wx, wy) & waypoint)) ->
            function
            | 'F', am ->
                let x' = (wx - sx) * am
                let y' = (wy - sy) * am
                (sx + x', sy + y'), (wx + x', wy + y')
            | op, am ->
                ship,
                match op with
                | ('L'
                | 'R') & op ->
                    let dist = dist ship waypoint
                    let angle = angle ship waypoint

                    let amop =
                        match op with
                        | 'L' -> id
                        | 'R' -> (*) -1.
                        | _ -> failwith ""

                    sx + dist * Math.Cos(angle + toRad (amop am)), sy + dist * Math.Sin(angle + toRad (amop am))
                | 'N' -> wx, wy + am
                | 'S' -> wx, wy - am
                | 'E' -> wx + am, wy
                | 'W' -> wx - am, wy
                | _ -> failwith "")
        ((0., 0.), (10., 1.))
    >> fst

let getManhattanDist (a: Int64) (b: Int64) = Math.Abs a + Math.Abs b

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
    |> fun (x, y) -> getManhattanDist (Convert.ToInt64 x) (Convert.ToInt64 y)

    // Output result
    |> printfn " ##### Result %d"

    0
