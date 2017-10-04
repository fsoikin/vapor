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

type ProcState = Running | Stopped | Toggling

type Proc = {
    Process: Types.Process
    LastLogTimestamp: System.DateTime
    State: ProcState
    Log: (System.DateTime * string) list
}

type Model = {
    Procs: Proc list
    HideLog: Types.ProcessName list
}

type Message =
    | FetchProcs
    | ProcsFetched of Types.Process list
    | ToggleShowLog of Proc
    | FetchAllLogs
    | FetchLog of Proc
    | LogFetched of Proc * (System.DateTime * string) list
    | Toggle of Proc
    | Err of System.Exception

let procColor idx =
    let idx = idx + 1
    let iff d = if idx &&& d = d then "aa" else "00"
    box <| sprintf "#%s%s%s" (iff 4) (iff 2) (iff 1)

let formatTime (t: System.DateTime) =
    sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d" t.Year t.Month t.Day t.Hour t.Minute t.Second t.Millisecond

let showLog model proc = not (model.HideLog |> List.contains proc.Process.Name)

let procView model dispatch idx proc =
    let toggle = OnClick <| fun _ -> dispatch <| Toggle proc
    let state =
        match proc.State with
        | Stopped -> "stopped"
        | Running -> "started"
        | Toggling -> "toggling"
    let pid = match proc.Process.State with | Types.Running pid -> string pid | _ -> ""
    let showLog = if showLog model proc then "visible" else "invisible"
    div [ ClassName "proc columns" ]
        [ div [ClassName "thumb column is-one-quarter"; Style [BackgroundColor <| procColor idx]]
              [ div [ ClassName <| "tile " + state; toggle ] []
                div [ ClassName <| "tile " + showLog; OnClick (fun _ -> dispatch <| ToggleShowLog proc) ] []
              ]
          div [ClassName "column"]
              [ div [ ClassName "name"; toggle; Style [Color <| procColor idx] ] [str proc.Process.Name]
                div [ ClassName "pid"; toggle  ] [str pid]
              ]
        ]

type LogEntry = { SourceProc: Proc; ProcIndex: int; Time: System.DateTime; Line: string }

let logEntryView e =
    tr [ ClassName "log-line"; Style [Color <| procColor e.ProcIndex] ]
        [ td [ ClassName "source" ] [ str e.SourceProc.Process.Name ]
          td [ ClassName "time" ]   [ str (formatTime e.Time)]
          td [ ClassName "text" ]   [ str e.Line ]
        ]

let view model dispatch =
    let tuple x y = x, y
    let mergedLogs =
        [for pIdx, p in model.Procs |> List.mapi tuple do
            if showLog model p then
                for lIdx, (t, l) in p.Log |> List.mapi tuple ->
                    lIdx, { SourceProc = p; ProcIndex = pIdx; Time = t; Line = l } ]
        |> List.sortByDescending (fun (i, e) -> e.Time, i)
        |> List.map snd
        |> List.truncate 200

    div
        []
        [ div
            [ ClassName "section" ]
            [ div
                [ ClassName "container" ]
                [ div
                    [ ClassName "tile is-ancestor" ]
                    [ div [ ClassName "tile is-vertical is-1" ]
                          (model.Procs |> List.mapi (procView model dispatch))
                      div [ ClassName "tile" ]
                          [ table []
                                [ tbody
                                    [ ClassName "log-lines tile is-vertical" ]
                                    (mergedLogs |> List.map logEntryView)
                                ]
                          ]
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

let hideLogKey = "HideLog_646fb22a-fe70-47f3-96fa-6a336e614b1a"

let init() =
    let hideLog =
        match Browser.localStorage.getItem hideLogKey with
            | :? string as s -> s.Split ',' |> List.ofArray
            | _ -> []
    { Procs = []; HideLog = hideLog }, Cmd.batch [Cmd.ofMsg FetchProcs; Cmd.ofSub poll]

let mergeProcs existing incoming =
    [for i:Types.Process in incoming ->
        let state =
            match i.State with
            | Types.Running _ -> Running
            | Types.Stopped -> Stopped
            | Types.Stopping -> Toggling

        match existing |> List.tryFind (fun x -> x.Process.Name = i.Name) with
        | Some ex -> { ex with Process = i; State = state }
        | None -> { Process = i; State = state; LastLogTimestamp = Types.minTime; Log = [] }
    ]

let updateProc model newProc =
    { model with Procs = [for p in model.Procs -> if p.Process.Name = newProc.Process.Name then newProc else p] }

let mergeLog existing incoming =
    match incoming with
    | [] ->
        existing
    | (time,i)::_ ->
        (existing |> List.takeWhile (fun (t,_) -> t < time)) @ incoming

let toggleProc model proc makeRequest =
    updateProc model { proc with State = Toggling }, Cmd.ofPromise makeRequest proc.Process.Name (fun _ -> FetchProcs) Err

let update msg model =
    match msg with
    | FetchProcs ->
        model, Cmd.ofPromise Fetch.list () ProcsFetched Err
    | ProcsFetched procs ->
        { model with Procs = mergeProcs model.Procs procs }, Cmd.none
    | ToggleShowLog proc ->
        let isShowing = showLog model proc
        let newHideLog =
            if isShowing
            then proc.Process.Name :: model.HideLog
            else model.HideLog |> List.filter ((<>) proc.Process.Name)
        Browser.localStorage.setItem( hideLogKey, String.concat "," newHideLog )
        { model with HideLog = newHideLog }, Cmd.none
    | FetchAllLogs ->
        model, model.Procs |> List.map (FetchLog >> Cmd.ofMsg) |> Cmd.batch
    | FetchLog proc ->
        model, Cmd.ofPromise (Fetch.log proc.Process.Name) proc.LastLogTimestamp (fun ls -> LogFetched (proc, ls)) Err
    | LogFetched (proc, logs) ->
        let newLogs = mergeLog proc.Log logs
        let latest = if List.isEmpty logs then Types.minTime else logs |> List.map fst |> List.max
        Fable.Import.JS.console.log (string latest)
        // hack:
        let cmd = if logs |> Seq.exists (fun (_, l) -> l.Contains "Stopped") then Cmd.ofMsg FetchProcs else Cmd.none
        updateProc model { proc with Log = newLogs; LastLogTimestamp = latest }, cmd
    | Toggle proc ->
        match proc.State with
        | Running -> toggleProc model proc Fetch.stop
        | Stopped -> toggleProc model proc Fetch.start
        | Toggling -> model, Cmd.none
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
