module Vapor.Main

open Suave
open Suave.Filters
open Suave.Operators

[<AutoOpen>]
module Json =
    let private jsonConverter = Fable.JsonConverter()
    let toJson x = Newtonsoft.Json.JsonConvert.SerializeObject( x, jsonConverter )

module FS =
    let home = System.AppContext.BaseDirectory
    let resolve segments = System.IO.Path.Combine( Array.ofList <| home::segments ) |> System.IO.Path.GetFullPath

let homeDir = FS.resolve ["Content"]

let getLogs root (procs:string, ticks) =
    let logs =
        [for proc in procs.Split ',' do
            let log = Process.Ops.log root proc (System.DateTime ticks)
            if not <| List.isEmpty log then yield proc, log
        ]
    Successful.OK << toJson <| logs

let app (cfg: Process.Stats.Config) =
    let files =
        if cfg.debug then
            Writers.setHeader "Cache-Control" "max-age=1" >=> context (fun _ -> Files.browseHome)
        else
            Files.browseHome

    choose [
        GET  >=> path "/api/list"          >=> request (fun _ -> Process.Stats.list cfg |> toJson |> Successful.OK)
        POST >=> path "/api/gc"            >=> request (fun _ -> Process.Ops.collectGarbage cfg |> toJson |> Successful.OK)
        GET  >=> pathScan "/api/log/%s/%d" (getLogs cfg)
        POST >=> pathScan "/api/start/%s"  (Process.Ops.start cfg >> toJson >> Successful.OK)
        POST >=> pathScan "/api/stop/%s"   (Process.Ops.stop cfg >> toJson >> Successful.OK)

        GET >=> choose
            [ path "/" >=> Files.file (homeDir + "/index.html")
              files ]

        RequestErrors.NOT_FOUND "Not Found"
    ]

let run (cfg: Process.Stats.Config) port =
    printfn "HomeDir = %s" homeDir
    printfn "Root = %s" cfg.root

    if not (System.IO.Directory.Exists cfg.root) then
        printfn "Directory '%s' doesn't exist" cfg.root
        1
    else
        app cfg
        |> startWebServer
            { defaultConfig with
                homeFolder = Some homeDir
                bindings =
                    match port with
                    | Some p -> [ HttpBinding.create HTTP System.Net.IPAddress.Loopback p ]
                    | _ -> defaultConfig.bindings
            }
        0


[<AutoOpen>]
module Options =
    open Argu

    type Options =
        | [<ExactlyOnce>] Root of path:string
        | [<AltCommandLine "-p"; Unique>] Port of port:System.UInt16
        | [<Unique>] Shell of shell:string
        | [<AltCommandLine "-d"; Unique>] Debug
    with interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Root _ -> "Root directory, where the `procs` file is located"
                | Port _ -> "Port on which to listen (if not specified will be chosen automatically)"
                | Shell _ -> "Shell to use for execution together with its option for executing a command, e.g. `bash -c`.\nDefault is `bash` on Unix and `powershell` on Windows."
                | Debug -> "Debug mode"

    let parser = ArgumentParser.Create<Options>(programName = "vapor")


let guessShell =
    match System.Environment.OSVersion.Platform with
    | System.PlatformID.Unix | System.PlatformID.MacOSX ->
        "bash -c"
    | _ (* Assuming Windows *) ->
        "powershell -Command"


[<EntryPoint>]
let main argv =
    try
        let opts = Options.parser.Parse argv
        let cfg : Process.Stats.Config = {
            root = opts.GetResult <@Root@>
            debug = opts.Contains <@Debug@>
            shell = Option.defaultValue guessShell <| opts.TryGetResult <@Shell@>
        }
        run cfg (opts.TryGetResult <@Port@>)
    with
        | :? Argu.ArguParseException as e -> printfn "%s" e.Message; 1
