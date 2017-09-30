module Vapor.Fetch

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.JS
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Fetch.Fetch_types

let inline private fetch url parms = promise {
    let! r = Fetch.fetch ("/api" + url) parms
    let! u = r.text()
    return ofJson<_> u
}

let inline private get url = fetch url [Method HttpMethod.GET]
let inline private post url = fetch url [Method HttpMethod.POST]

let list() : Types.Process list Promise = get "/list"
let start proc : unit Promise = post ("/start/" + proc)
let stop proc : unit Promise = post ("/stop/" + proc)
let log proc (afterTime: System.DateTime) : (System.DateTime * string) list Promise = get ("/log/" + proc + "/" + string afterTime.Ticks)
