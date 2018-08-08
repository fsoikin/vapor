module Vapor.Monitor
open System

type P = Diagnostics.Process
type SI = Diagnostics.ProcessStartInfo

module OS =
    open System.Runtime.InteropServices

    module private Windows =
        [<DllImport("kernel32.dll", SetLastError=true)>] extern bool GenerateConsoleCtrlEvent(int sigevent, int dwProcessGroupId);
        [<DllImport("kernel32.dll", SetLastError=true)>] extern bool AllocConsole();

        let _kill pid = GenerateConsoleCtrlEvent(0, pid) |> ignore
        let initConsole = AllocConsole >> ignore

    module private Unix =
        [<DllImport ("libc", SetLastError=true)>] extern unit kill (int pid, int signal);

        let _kill pid = kill(pid, 2 (* SIGINT *))
        let initConsole () = ()


    let _kill, initConsole =
        match System.Environment.OSVersion.Platform with
        | System.PlatformID.Unix | System.PlatformID.MacOSX ->
            Unix._kill, Unix.initConsole
        | _ (* Assuming Windows *) ->
            Windows._kill, Windows.initConsole

    let killProcess (p: P) =
        let rec loop() = async {
            _kill p.Id
            do! Async.Sleep 100
            return! loop()
        }
        Async.Start <| loop()


let run (cfg: Process.Stats.Config) proc =
    let file = Files.procFile cfg.root proc
    let pidFile, stopFile, logFile = file Files.Pid, file Files.Stop, file Files.Log

    let needToStop() = IO.File.Exists stopFile
    let log line = Files.append logFile (sprintf "%s: %s" (DateTime.Now.ToString Types.timeFormat) line)

    Console.CancelKeyPress.AddHandler ( fun _ (e: System.ConsoleCancelEventArgs) -> e.Cancel <- true )
    OS.initConsole()

    let rec pipeOutput (p: P) (stream: IO.StreamReader) =
        async {
            let! line = Async.AwaitTask <| stream.ReadLineAsync()
            log line
            if not p.HasExited then
                return! pipeOutput p stream
        }

    let shellFile, shellCmd =
        match cfg.shell.Split( [|' '|], 2 ) with
        | [|cmd; opt|] -> cmd, opt
        | [|cmd|] -> cmd, ""
        | _ -> cfg.shell, ""

    let si (cmd: string) = SI( shellFile, sprintf """%s "%s" """ shellCmd (cmd.Replace("\\","\\\\")) )

    let rec waitForStopSignal kill (p: P) =
        async {
            do! Async.Sleep 500
            if needToStop() then
                log ("Stopping " + proc)
                match kill with
                | None -> OS.killProcess p
                | Some k -> ignore <| P.Start (si k)
            elif p.HasExited then
                log ("Crashed: " + proc)
            else
                return! waitForStopSignal kill p
        }

    Process.Stats.find cfg proc
    |> Option.iter (fun pr ->
        log ("Starting " + proc)

        let p =
            let s = si pr.Cmd
            s.RedirectStandardInput <- true
            s.RedirectStandardOutput <- true
            s.RedirectStandardError <- true
            P.Start s

        Files.write pidFile (string p.Id)

        [ pipeOutput p p.StandardOutput
          pipeOutput p p.StandardError
          waitForStopSignal pr.Kill p ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

        log ("Stopped " + proc)
        Files.maybeDelete pidFile
    )

[<EntryPoint>]
let main argv =
    match argv with
    | [|root; shell; proc|] -> run { root = root; shell = shell; debug = false } proc
    | _ -> printfn "Bad args: %A" argv

    0
