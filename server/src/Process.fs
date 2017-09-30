module Vapor.Process

open System.IO
open Vapor.Types

[<AutoOpen>]
module private Private =
    type File = Pid | Log | Stop

    let procFile root proc kind =
        let ext =
            match kind with
            | Pid -> "pid"
            | Log -> "log"
            | Stop -> "stop"
        Path.ChangeExtension( Path.Combine(root, proc), ext )

    let maybeRead file = if File.Exists file then Some <| File.ReadAllText file else None
    let maybeDelete file = try File.Delete file with _ -> ()

    let activeWait op =
        let rec loop i =
            if i <= 0 then None
            else match op() with
                    | Some x -> Some x
                    | None -> System.Threading.Thread.Sleep 200; loop (i-1)

        loop 50

    let tryInt s = match System.Int32.TryParse s with (true, x) -> Some x | _ -> None

    let pid root proc = procFile root proc Pid |> maybeRead |> Option.bind tryInt

    let monitoringHostCode root =
        let initPath = Path.Combine(root, "init.ps1")
        let runInit = if File.Exists initPath then sprintf "& %s;" initPath else ""
        sprintf
            """
                param( $cmd, $pidfile, $stopfile, $logfile )

                function log($msg) { "$((Get-Date).ToString( '%s' )): $msg" | out-file -append $logfile -force }

                set-content $pidfile $PID
                log "Starting $cmd"
                $job = start-job -scriptblock { iex "powershell -command `"%s $args[0]`" 2>&1 3>&1 4>&1" | %%{ "$((Get-Date).ToString( '%s' )): $_" } | Out-file $args[1] -Append } `
                                 -argumentlist @($cmd, $logfile)

                while( -not (test-path $stopfile) ) { start-sleep -s 1 }

                log "Stopping $cmd"
                stop-job $job
                log "Stopped $cmd"
                del $pidfile
            """
            timeFormat runInit timeFormat

    let writeMonitoringScript root =
        let file = Path.Combine(root, ".monitor.ps1")
        File.WriteAllText(file, monitoringHostCode root)
        file

let tryParseTime s = match System.DateTime.TryParseExact( s, timeFormat, null, System.Globalization.DateTimeStyles.None ) with (true, x) -> Some x | _ -> None

let list root =
    [for proc in File.ReadAllLines (Path.Combine(root, "procs")) do
        let ps = proc.Split([|':'|], 2)
        if ps.Length = 2
        then yield { Name = ps.[0]; Cmd = ps.[1]; Pid = pid root ps.[0] }
    ]

let find root proc = list root |> List.tryFind (fun p -> p.Name = proc)

let start root proc =
    find root proc
    |> Option.filter (fun p -> p.Pid = None)
    |> Option.bind (fun p ->
        Log.log "Starting process %s ..." proc
        let pidFile = procFile root proc Pid
        let runFile = writeMonitoringScript root

        System.Diagnostics.ProcessStartInfo(
            "powershell",
            sprintf """-NonInteractive -WindowStyle Hidden -Command "& '%s' '%s' '%s' '%s' '%s'" """
                runFile
                (p.Cmd.Replace("\"", "`\""))
                pidFile
                (procFile root proc Stop)
                (procFile root proc Log),
            CreateNoWindow=true )
        |> System.Diagnostics.Process.Start
        |> ignore

        let pid = activeWait (fun() -> maybeRead pidFile)
        match pid with
            | Some pid -> Log.log "Started process %s with PID=%s" proc pid
            | None -> Log.log "Failed to start process %s" proc

        pid
    )

let stop root proc =
    let stopFile = procFile root proc Stop
    let pidFile = procFile root proc Pid

    maybeRead pidFile |> Option.iter (fun pid ->
        Log.log "Attempting to stop process %s with PID=%s ..." proc pid

        File.WriteAllText(stopFile, "42")
        match activeWait (fun() -> if File.Exists pidFile then None else Some()) with
            | Some() -> Log.log "Stopped process %s with PID=%s" proc pid
            | None -> Log.log "Unable to stop process %s with PID=%s" proc pid
    )

    maybeDelete pidFile
    maybeDelete stopFile

let log root proc afterTime =
    let splitTime (s: string) =
        Some()
        |> Option.filter (fun _ -> s.Length > timeFormat.Length )
        |> Option.bind (fun _ -> tryParseTime (s.Substring(0, timeFormat.Length)))
        |> Option.map (fun t -> t, s.Substring(timeFormat.Length+1).Trim())
        |> Option.defaultValue (minTime, s)

    // TODO: refine this to not load the whole file
    maybeRead (procFile root proc Log) |> Option.defaultValue ""
    |> (fun s -> s.Split '\n')
    |> Seq.map splitTime
    |> Seq.filter (fun (t,s) -> t >= afterTime)
    |> List.ofSeq
