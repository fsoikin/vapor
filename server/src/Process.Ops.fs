module Vapor.Process.Ops

open System.IO
open Vapor.Types
open Vapor.Files
open Vapor.Process.Stats
module Log = Vapor.Log

let tryParseTime s = match System.DateTime.TryParseExact( s, timeFormat, null, System.Globalization.DateTimeStyles.None ) with (true, x) -> Some x | _ -> None

let writeMonitorFile() =
    let path = Path.Combine( Path.GetTempPath(), System.Guid.NewGuid().ToString() )
    Directory.CreateDirectory path |> ignore
    for f in Directory.GetFiles( System.AppDomain.CurrentDomain.BaseDirectory, "monitor.*" ) do
        File.Copy( f, Path.Combine(path, Path.GetFileName f) )
    Path.Combine( path, "monitor.dll" )

let start cfg proc =
    find cfg proc
    |> Option.filter (fun p -> p.State = Stopped)
    |> Option.bind (fun p ->
        Log.log "Starting process %s ..." proc
        let pidFile = procFile cfg.root proc Pid
        let runFile = writeMonitorFile()

        let args = sprintf """"%s" "%s" "%s" "%s" """ runFile (cfg.root.Replace("\\", "/")) (cfg.shell.Replace("\\", "\\\\")) proc
        Log.log "%s" args

        let p =
            System.Diagnostics.ProcessStartInfo( "dotnet", args, CreateNoWindow=true, UseShellExecute=false, RedirectStandardOutput=true )
            |> System.Diagnostics.Process.Start

        if cfg.debug then
            Async.Start <| async {
                while not p.HasExited do
                    let! line = p.StandardOutput.ReadLineAsync() |> Async.AwaitTask
                    Log.log "Monitor says: %s" line
            }

        let pid = activeWait (fun() -> maybeRead pidFile)
        match pid with
            | Some pid -> Log.log "Started process %s with PID=%s" proc pid
            | None -> Log.log "Failed to start process %s" proc

        pid
    )

let stop cfg proc =
    let stopFile = procFile cfg.root proc Stop
    let pidFile = procFile cfg.root proc Pid

    maybeRead pidFile |> Option.iter (fun pid ->
        Log.log "Attempting to stop process %s with PID=%s ..." proc pid

        File.WriteAllText(stopFile, "42")
        match activeWait (fun() -> if File.Exists pidFile then None else Some()) with
            | Some() -> Log.log "Stopped process %s with PID=%s" proc pid
            | None -> Log.log "Unable to stop process %s with PID=%s" proc pid
    )

    maybeDelete stopFile

let log cfg proc afterTime =
    let splitTime (s: string) =
        Some()
        |> Option.filter (fun _ -> s.Length > timeFormat.Length )
        |> Option.bind (fun _ -> tryParseTime (s.Substring(0, timeFormat.Length)))
        |> Option.map (fun t -> t, s.Substring(timeFormat.Length+1).Trim())
        |> Option.defaultValue (minTime, s)

    // TODO: refine this to not load the whole file
    maybeRead (procFile cfg.root proc Log) |> Option.defaultValue ""
    |> (fun s -> s.Split '\n')
    |> Seq.map splitTime
    |> Seq.filter (fun (t,s) -> t > afterTime)
    |> List.ofSeq

let private isStopped pid =
    try isNull <| System.Diagnostics.Process.GetProcessById pid
    with _ -> true

let collectGarbage cfg =
    for p in list cfg do
        match p.State with
        | Running pid when isStopped pid ->
            Log.log "Purging PID file for '%s' #%d, because it wasn't running." p.Name pid
            maybeDelete (procFile cfg.root p.Name Pid)
        | _ ->
            ()
