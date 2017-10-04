module Vapor.Process.Stats

open System.IO
open Vapor.Types
open Vapor.Files

let state root proc =
    let pid = procFile root proc Pid |> maybeRead |> Option.bind tryInt
    match pid, File.Exists (procFile root proc Stop) with
    | Some pid, true -> Stopping
    | Some pid, false -> Running pid
    | None, _ -> Stopped

let list root =
    [for proc in File.ReadAllLines (Path.Combine(root, "procs")) do
        let ps = proc.Split([|':'|], 2)
        if ps.Length = 2
        then yield { Name = ps.[0]; Cmd = ps.[1]; State = state root ps.[0] }
    ]

let find root proc = list root |> List.tryFind (fun p -> p.Name = proc)
