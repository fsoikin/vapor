module Vapor.App

open Elmish
open Fable.Core
open JsInterop
open Fable.Import
open Fable.Helpers.React
open Props

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
    | LogsFetched of (Types.ProcessName * (System.DateTime * string) list) list
    | Toggle of Proc
    | Err of System.Exception

let minTime = System.DateTime.Now.AddDays -1.

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

let rec mergeSort cmp xss = seq {
    let xx =
        xss
        |> Seq.choose (function
            | x::rest -> Some (x, rest)
            | [] -> None)
        |> Seq.indexed
        |> Seq.toList

    if not <| List.isEmpty xx then
        let (minIdx, (minItem, _)) = xx |> Seq.minBy (snd >> fst >> cmp)
        yield minItem
        yield! mergeSort cmp (xx |> Seq.map (fun (idx,(x,rest)) -> if idx = minIdx then rest else (x::rest)))
}

let view model dispatch =
    let mergedLogs =
        model.Procs
        |> Seq.indexed
        |> Seq.filter (snd >> showLog model)
        |> Seq.map (fun (pIdx, p) -> [for t, l in p.Log -> { SourceProc = p; ProcIndex = pIdx; Time = t; Line = l } ])
        |> mergeSort (fun x -> -x.Time.Ticks)
        |> Seq.map logEntryView
        |> Seq.truncate 200
        |> Seq.toList

    div
        []
        [ div
            [ ClassName "section" ]
            [ div
                [ ClassName "container" ]
                [ div
                    [ ClassName "tile is-ancestor" ]
                    [ div [ ClassName "tile is-vertical is-1"; Style [Position "relative"] ]
                          (model.Procs |> List.mapi (procView model dispatch))
                      div [ ClassName "tile is-vertical is-11" ]
                          [ table [Style [Width "100%"]]
                                [ tbody
                                    [ ClassName "log-lines tile is-vertical" ]
                                    mergedLogs
                                ]
                          ]
                    ]
                ]
            ]
        ]

let poll dispatch =
    Fable.Import.Browser.window.setInterval(
        box (fun() -> dispatch FetchAllLogs),
        box 1000
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
        | None -> { Process = i; State = state; LastLogTimestamp = minTime; Log = [] }
    ]

let mergeLog existing incoming =
    (List.rev incoming) @ existing

let safeMax defaultValue s =
    if Seq.isEmpty s then defaultValue
    else Seq.max s

let mergeLogs model ls =
    let ps =
        [for p in model.Procs ->
            match ls |> List.tryFind (fst >> ((=) p.Process.Name)) with
            | Some (_, logs) ->
                let newLog = mergeLog p.Log logs
                { p with
                    Log = newLog
                    LastLogTimestamp = newLog |> List.tryHead |> Option.map fst |> Option.defaultValue p.LastLogTimestamp }
            | None ->
                p
        ]
    { model with Procs = ps }

let updateProc model newProc =
    { model with Procs = [for p in model.Procs -> if p.Process.Name = newProc.Process.Name then newProc else p] }

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
        let latestTimestamp = seq { for p in model.Procs -> p.LastLogTimestamp } |> safeMax Types.minTime
        let names = [for p in model.Procs -> p.Process.Name]
        model, Cmd.ofPromise (Fetch.logs names) latestTimestamp LogsFetched Err
    | LogsFetched ls ->
        // hack:
        let cmd = if ls |> Seq.collect snd |> Seq.exists (fun (_, l) -> l.Contains "Stopped") then Cmd.ofMsg FetchProcs else Cmd.none
        mergeLogs model ls, cmd
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
open Fable.PowerPack

Fetch.collectGarbage() |> Promise.start

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.run
