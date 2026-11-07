/// Writes goldens without disturbing the line endings of a golden whose content is unchanged.
module GoldenFiles

open System.IO

/// `text` with every carriage return removed.
let normalise (text: string) : string = text.Replace("\r", "")

/// Writes `content` to the golden at `path` when the golden is absent or its content, compared with
/// carriage returns removed, differs. A rewritten golden keeps its line endings: CRLF when it holds
/// any, as a `core.autocrlf` checkout leaves it, and LF otherwise.
let write (path: string) (content: string) : unit =
    let normalised = normalise content

    match File.Exists path with
    | true ->
        let existing = File.ReadAllText path

        match normalise existing = normalised with
        | true -> ()
        | false ->
            match existing.Contains "\r\n" with
            | true -> File.WriteAllText(path, normalised.Replace("\n", "\r\n"))
            | false -> File.WriteAllText(path, normalised)
    | false ->
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllText(path, normalised)
