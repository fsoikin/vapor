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

let app root debug =
    let files =
        if debug then
            Writers.setHeader "Cache-Control" "max-age=1" >=> context (fun _ -> Files.browseHome)
        else
            Files.browseHome

    choose [
        GET  >=> path "/api/list"          >=> request (fun _ -> Process.Stats.list root |> toJson |> Successful.OK)
        POST >=> path "/api/gc"            >=> request (fun _ -> Process.Ops.collectGarbage root |> toJson |> Successful.OK)
        GET  >=> pathScan "/api/log/%s/%d" (getLogs root)
        POST >=> pathScan "/api/start/%s"  (Process.Ops.start debug root >> toJson >> Successful.OK)
        POST >=> pathScan "/api/stop/%s"   (Process.Ops.stop root >> toJson >> Successful.OK)

        GET >=> choose
            [ path "/" >=> Files.file (homeDir + "/index.html")
              files ]

        RequestErrors.NOT_FOUND "Not Found"
    ]

let run root port debug =
    printfn "HomeDir = %s" homeDir
    printfn "Root = %s" root

    if not (System.IO.Directory.Exists root) then
        printfn "Directory '%s' doesn't exist" root
        1
    else
        app root debug
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
        | [<AltCommandLine "-d"; Unique>] Debug
    with interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Root _ -> "Root directory, where the `procs` file is located"
                | Port _ -> "Port on which to listen (if not specified will be chosen automatically)"
                | Debug -> "Debug mode"

    let parser = ArgumentParser.Create<Options>(programName = "vapor")


[<EntryPoint>]
let main argv =
    try
        let opts = Options.parser.Parse argv
        run (opts.GetResult <@Root@>) (opts.TryGetResult <@Port@>) (opts.Contains <@Debug@>)
    with
        | :? Argu.ArguParseException as e -> printfn "%s" e.Message; 1
