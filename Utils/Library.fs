#nowarn "40"

namespace Utils

open TextCopy
open System.Collections.Generic

module Pair =
    let map f (a, b) = (f a, f b)

module Functional =
    let flip f a b = f b a
    let thd (a, b, c) = c
    let fth (a, b, c, d) = d
    let both f a = f a a
    let tuple2 a b = a, b
    let tuple3 a b c = a, b, c
    let tuple4 a b c d = a, b, c, d
    let tuple5 a b c d e = a, b, c, d, e
    let tuple6 a b c d e f = a, b, c, d, e, f

module Math =
    let testBetween f t v =
        if f <= v && v <= t then
            Some(v)
        else
            None

module Clipboard =
    let get () = ClipboardService.GetText()
    let set = ClipboardService.SetText
    let apply f = get () |> f |> set

module String =
    let newline = System.Environment.NewLine

    let toSeq (separator: string) converter (source: string) =
        source.Split([| separator |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (converter)

    let split s = toSeq s id >> Seq.toList
    let linesToSeq converter = toSeq newline converter
    let linesToStringSeq = linesToSeq id
    let linesToStringList = linesToStringSeq >> Seq.toList
    let linesToIntSeq = linesToSeq int
    let csvToStringSeq = toSeq "," id
    let csvToStringList = csvToStringSeq >> Seq.toList
    let csvToIntSeq = toSeq "," int
    let csvToIntList = csvToIntSeq >> Seq.toList
    let replace (oldValue: string) newValue (source: string) = source.Replace(oldValue, newValue)
    let removePart part = replace part ""

    let rec repeat =
        Functional.flip
        <| (fun source ->
            function
            | gt0 when gt0 > 0 -> source + (repeat (gt0 - 1) source)
            | _ -> "")

module List =
    let replace<'T> ix (sub: 'T) =
        List.mapi (fun ix0 x -> if ix0 = ix then sub else x)

    let slice ix1 ix2 list = list |> List.skip ix1 |> List.take ix2

    let toTuple =
        function
        | [ a; b ] -> (a, b)
        | _ -> failwith ""

    let toTuple3 =
        function
        | [ a; b; c ] -> (a, b, c)
        | _ -> failwith ""

module Seq =
    let collecti f = Seq.mapi f >> Seq.collect id
    let choosei f = Seq.mapi f >> Seq.choose id
    let choose2 f a b = Seq.map2 f a b |> Seq.choose id
    let all f = Seq.exists (f >> (not)) >> (not)

    let fromTuple (a, b) =
        seq {
            yield a
            yield b
        }

    let fromTuple3 (a, b, c) =
        seq {
            yield a
            yield b
            yield c
        }

module Dict =
    let keys (source: IDictionary<'a, 'b>) = source.Keys

module Boolean =
    let toOption source =
        (=) true
        >> function
        | true -> Some source
        | _ -> None

    let testEquality a b = a = b |> toOption a
    let testEqualityFor result a = (=) a >> toOption result
