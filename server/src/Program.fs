module Vapor.Main

open Suave
open Suave.Filters
open Suave.Operators
open Microsoft.Extensions.Configuration

[<AutoOpen>]
module Json =
    let private jsonConverter = Fable.JsonConverter()
    let toJson x = Newtonsoft.Json.JsonConvert.SerializeObject( x, jsonConverter )

module FS =
    let home = System.AppContext.BaseDirectory
    let resolve segments = System.IO.Path.Combine( Array.ofList <| home::segments ) |> System.IO.Path.GetFullPath

module Config =
    type Config = { Root: string; Debug: bool }
    let config =
        let root = ConfigurationBuilder().SetBasePath(FS.home).AddJsonFile("appsettings.json").Build()
        { Root = root.["Root"]; Debug = root.["Debug"] = "true" }

let homeDir = FS.resolve ["Content"]
let root = FS.resolve [Config.config.Root]

let files =
    if Config.config.Debug then
        Writers.setHeader "Cache-Control" "max-age=1" >=> context (fun c -> Files.browseHome)
    else
        Files.browseHome

let getLog (proc, ticks) =
    Successful.OK << toJson <| Process.log root proc (System.DateTime ticks)

let app =
    choose [
        GET  >=> path "/api/list"          >=> request (fun _ -> Process.list root |> toJson |> Successful.OK)
        GET  >=> pathScan "/api/log/%s/%d" getLog
        POST >=> pathScan "/api/start/%s"  (Process.start root >> toJson >> Successful.OK)
        POST >=> pathScan "/api/stop/%s"   (Process.stop root >> toJson >> Successful.OK)

        GET >=> choose
            [ path "/" >=> Files.file (homeDir + "/index.html")
              files ]

        RequestErrors.NOT_FOUND "Not Found"
    ]

let bindingsFromPort p =
    match Option.map Sockets.Port.TryParse p with
    | Some (true, p) -> [ HttpBinding.create HTTP System.Net.IPAddress.Loopback p ]
    | _ -> defaultConfig.bindings

[<EntryPoint>]
let main argv =
    printfn "HomeDir = %s" homeDir
    app
    |> startWebServer
        { defaultConfig with
            homeFolder = Some homeDir
            bindings = bindingsFromPort (Seq.tryHead argv)
        }
    0
