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
        if ps.Length = 2
        then yield { Name = ps.[0]; Cmd = ps.[1]; State = state cfg ps.[0] }
    ]

let find cfg proc = list cfg |> List.tryFind (fun p -> p.Name = proc)
