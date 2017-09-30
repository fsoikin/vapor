module Vapor.App

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props

importAll "../sass/main.sass"

type Proc = {
    Process: Types.Process
    ShowLog: bool
    LastLogTimestamp: System.DateTime
    Log: (System.DateTime * string) list
}

type Model = {
    Procs: Proc list
}

type Message =
    | FetchProcs
    | ProcsFetched of Types.Process list
    | ToggleShowLog of Proc
    | FetchAllLogs
    | FetchLog of Proc
    | LogFetched of Proc * (System.DateTime * string) list
    | Start of Proc
    | Stop of Proc
    | Err of System.Exception

let procColor idx =
    let idx = idx + 1
    let iff d = if idx &&& d = d then "ff" else "00"
    box <| sprintf "%s%s%s" (iff 4) (iff 2) (iff 1)

let procView dispatch idx proc =
    let toggle = OnClick <| fun _ -> dispatch <| if proc.Process.Pid = None then Start proc else Stop proc
    div [ ClassName "proc"
          Style [BackgroundColor <| procColor idx]
        ]
        [ div []
              [ div [ ClassName "name"; toggle ] [str proc.Process.Name]
                div [ ClassName "pid"; toggle  ] [str (string proc.Process.Pid)]
              ]
          div [ ClassName "toggleShow"; OnClick (fun _ -> dispatch <| ToggleShowLog proc) ]
              [ str <| if proc.ShowLog then "V" else "X" ]
        ]

type LogEntry = { SourceProc: Proc; ProcIndex: int; Time: System.DateTime; Line: string }

let logEntryView e =
    div [ ClassName "log-line"; Style [Color <| procColor e.ProcIndex] ]
        [ div [ ClassName "source" ] [ str e.SourceProc.Process.Name ]
          div [ ClassName "time" ]   [ str (e.Time.ToString Types.timeFormat)]
          div [ ClassName "text" ]   [ str e.Line ]
        ]

let view model dispatch =

    let mergedLogs =
        [for idx, p in model.Procs |> List.mapi (fun i p -> i,p) do
            if p.ShowLog then
                for t, l in p.Log ->
                    { SourceProc = p; ProcIndex = idx; Time = t; Line = l } ]
        |> List.sortByDescending (fun e -> e.Time)
        |> List.truncate 200

    div
        []
        [ div
            [ ClassName "section" ]
            [ div
                [ ClassName "container" ]
                [ div
                    [ ClassName "columns" ]
                    [ div
                        [ ClassName "column" ]
                        (model.Procs |> List.mapi (procView dispatch))
                      div
                        [ ClassName "column" ]
                        (mergedLogs |> List.map logEntryView)
                    ]
                ]
            ]
        ]

let poll dispatch =
    Fable.Import.Browser.window.setInterval(
        box (fun() -> dispatch FetchAllLogs),
        box 5000
    )
    |> ignore

let init() = { Procs = [] }, Cmd.batch [Cmd.ofMsg FetchProcs; Cmd.ofSub poll]

let mergeProcs existing incoming =
    [for i:Types.Process in incoming ->
        match existing |> List.tryFind (fun x -> x.Process.Name = i.Name) with
        | Some ex -> { ex with Process = i }
        | None -> { Process = i; ShowLog = true; LastLogTimestamp = Types.minTime; Log = [] }
    ]

let updateProc model newProc =
    { model with Procs = [for p in model.Procs -> if p.Process.Name = newProc.Process.Name then newProc else p] }

let mergeLog existing incoming =
    match incoming with
    | [] ->
        existing
    | (time,i)::_ ->
        (existing |> List.takeWhile (fun (t,_) -> t < time)) @ incoming

let update msg model =
    match msg with
    | FetchProcs ->
        model, Cmd.ofPromise Fetch.list () ProcsFetched Err
    | ProcsFetched procs ->
        { Procs = mergeProcs model.Procs procs }, Cmd.none
    | ToggleShowLog proc ->
        updateProc model { proc with ShowLog = not proc.ShowLog }, Cmd.none
    | FetchAllLogs ->
        model, model.Procs |> List.map (FetchLog >> Cmd.ofMsg) |> Cmd.batch
    | FetchLog proc ->
        model, Cmd.ofPromise (Fetch.log proc.Process.Name) proc.LastLogTimestamp (fun ls -> LogFetched (proc, ls)) Err
    | LogFetched (proc, logs) ->
        let newLogs = mergeLog proc.Log logs
        let latest = logs |> List.tryLast |> Option.map fst |> Option.defaultValue Types.minTime
        updateProc model { proc with Log = newLogs; LastLogTimestamp = latest }, Cmd.none
    | Start proc ->
        model, Cmd.ofPromise Fetch.start proc.Process.Name (fun _ -> FetchProcs) Err
    | Stop proc ->
        model, Cmd.ofPromise Fetch.stop proc.Process.Name (fun _ -> FetchProcs) Err
    | Err e ->
        Fable.Import.JS.console.log (string e)
        model, Cmd.none


open Elmish.React
open Elmish.Debug

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
