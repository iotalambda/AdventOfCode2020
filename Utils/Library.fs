namespace Utils

open TextCopy

module Clipboard =
    let get () = ClipboardService.GetText()
    let set = ClipboardService.SetText
    let apply f = get () |> f |> set
