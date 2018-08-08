module Vapor.Process.Stats

open System.IO
open Vapor.Types
open Vapor.Files

type Config = { root: string; shell: string; debug: bool }

let state cfg proc =
    let pid = procFile cfg.root proc Pid |> maybeRead |> Option.bind tryInt
    match pid, File.Exists (procFile cfg.root proc Stop) with
    | Some _, true -> Stopping
    | Some pid, false -> Running pid
    | None, _ -> Stopped

let list cfg =
    [for proc in File.ReadAllLines (Path.Combine(cfg.root, "procs")) do
        let ps = proc.Split([|':'|], 2)
        if ps.Length = 2 then
            let (run, kill) =
                let killIdx = ps.[1].IndexOf( "kill:" )
                if killIdx > 0 then (ps.[1].Substring(0, killIdx), Some (ps.[1].Substring(killIdx+5)))
                else (ps.[1], None)
            yield { Name = ps.[0]; Cmd = run; Kill = kill; State = state cfg ps.[0] }
    ]

let find cfg proc = list cfg |> List.tryFind (fun p -> p.Name = proc)
