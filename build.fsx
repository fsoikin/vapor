#r @"packages/build/FAKE/tools/FakeLib.dll"
open System
open Fake

let root = __SOURCE_DIRECTORY__
let config = getBuildParamOrDefault "Config" "Debug"
let serverBin = root </> "built"
let skipNpmInstall = getBuildParamOrDefault "SkipNpmInstall" "true" = "true"

let buildServer() =
  DotNetCli.Build (fun p -> { p with Project = "server/src/vapor.fsproj"; Configuration = config; Output = serverBin })
  Fake.FileUtils.cp "server/src/appsettings.json" serverBin

let deployContent() =
    let content = sprintf "%s/Content" serverBin
    Fake.FileSystemHelper.ensureDirectory content

    !! "client/public/*.html"
    ++ "client/public/*.js"
    ++ "client/public/*.js.map"
    |> Seq.iter (Fake.FileHelper.CopyFileWithSubfolder "client/public" content)

Target "BuildServer" buildServer

Target "BuildMonitor" <| fun() ->
  DotNetCli.Build (fun p -> { p with Project = "monitor/monitor.fsproj"; Configuration = config; Output = serverBin })

Target "BuildClient" <| fun _ ->
    if not skipNpmInstall then
        NpmHelper.Npm (fun p -> {p with Command = NpmHelper.Install NpmHelper.Standard; WorkingDirectory = root </> "client" })
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = root </> "client/src" }) "fable yarn-run build"

Target "DeployContent" deployContent

Target "Clean" <| fun _ ->
  Fake.FileUtils.rm_rf serverBin

Target "Build" DoNothing

Target "Watch" <| fun() ->
  use watchClientContent =
    !! "client/public/**/*"
    |> WatchChanges (fun _ -> deployContent())

  use watchServerSrc =
    !! "server/src/**/*.fs"
    |> WatchChanges (fun _ -> buildServer())

  [ async { DotNetCli.RunCommand (fun p -> { p with WorkingDir = "client/src" }) "fable start"; return 0 }
    async { NpmHelper.Npm (fun p -> {p with Command = NpmHelper.Custom "run watch"; WorkingDirectory = root </> "client"}); return 0 }
    Shell.AsyncExec( "dotnet", args = sprintf "%s/Vapor.dll 8085" serverBin, dir = serverBin ) ]
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore

"BuildClient"
  ==> "BuildMonitor"
  ==> "BuildServer"
  ==> "DeployContent"
  ==> "Build"

RunTargetOrDefault "Build"
