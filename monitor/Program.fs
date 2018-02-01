module Vapor.Monitor
open System

type P = Diagnostics.Process
type SI = Diagnostics.ProcessStartInfo

module OS =

    [<AutoOpen>]
    module private Private =
        [<Runtime.InteropServices.DllImport("kernel32.dll", SetLastError=true)>]
        extern bool GenerateConsoleCtrlEvent(int sigevent, int dwProcessGroupId);

        [<Runtime.InteropServices.DllImport("kernel32.dll", SetLastError=true)>]
        extern bool AllocConsole();

    let killProcess (p: P) =
        let rec loop() = async {
            GenerateConsoleCtrlEvent( 0, p.Id ) |> ignore
            do! Async.Sleep 100
            return! loop()
        }
        Async.Start <| loop()

    let initConsole = AllocConsole >> ignore


let run root proc =
    let file = Files.procFile root proc
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

    let rec waitForStopSignal (p: P) =
        async {
            do! Async.Sleep 500
            if needToStop() then
                log ("Stopping " + proc)
                OS.killProcess p
            elif p.HasExited then
                log ("Crashed: " + proc)
            else
                return! waitForStopSignal p
        }

    Process.Stats.find root proc
    |> Option.iter (fun pr ->
        log ("Starting " + proc)

        let p =
            SI( "powershell", sprintf """-Command "%s" """ pr.Cmd,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true )
            |> P.Start

        Files.write pidFile (string p.Id)

        [ pipeOutput p p.StandardOutput
          pipeOutput p p.StandardError
          waitForStopSignal p ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

        log ("Stopped " + proc)
        Files.maybeDelete pidFile
    )

[<EntryPoint>]
let main argv =
    match argv with
    | [|root; proc|] -> run root proc
    | _ -> printfn "Bad args"

    0
