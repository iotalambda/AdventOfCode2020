namespace Utils

open TextCopy

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

module List =
    let replace<'T> ix (sub: 'T) =
        List.mapi (fun ix0 x -> if ix0 = ix then sub else x)

    let slice ix1 ix2 list = list |> List.skip ix1 |> List.take ix2

module Seq =
    let collecti f = Seq.mapi f >> Seq.collect id
    let choosei f = Seq.mapi f >> Seq.choose id
    let all f = Seq.exists (f >> (not)) >> (not)

module Pair =
    let map f (a, b) = (f a, f b)
    let sum (a, b) = a + b
    let multiply (a, b) = a * b

module Functional =
    let flip f a b = f b a
    let thd (a, b, c) = c
    let fth (a, b, c, d) = d
    let both f a = f a a
