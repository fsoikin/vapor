module Vapor.Files

open System.IO
open Vapor.Types

type File = Pid | Log | Stop

let procFile root proc kind =
    let ext =
        match kind with
        | Pid -> "pid"
        | Log -> "log"
        | Stop -> "stop"
    Path.ChangeExtension( Path.Combine(root, proc), ext )

let maybeRead file =
    if File.Exists file then
        use str = new FileStream( file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite )
        use rd = new StreamReader( str, System.Text.UTF8Encoding( false ) )
        Some <| rd.ReadToEnd()
    else
        None

let write file text = System.IO.File.WriteAllText( file, text )

let append file (text: string) =
    use str = new FileStream( file, FileMode.OpenOrCreate, FileAccess.Write, FileShare.ReadWrite )
    str.Seek( 0L, SeekOrigin.End ) |> ignore
    use wr = new StreamWriter( str, System.Text.UTF8Encoding( false ) )
    wr.WriteLine text

let maybeDelete file = try File.Delete file with _ -> ()

let activeWait op =
    let rec loop i =
        if i <= 0 then None
        else match op() with
                | Some x -> Some x
                | None -> System.Threading.Thread.Sleep 200; loop (i-1)

    loop 50

let tryInt s = match System.Int32.TryParse s with (true, x) -> Some x | _ -> None
