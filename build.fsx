#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake.YarnHelper
open Fake

let root = __SOURCE_DIRECTORY__
let config = getBuildParamOrDefault "Config" "Debug"
let serverBin = root </> "built"
let skipYarnInstall = getBuildParamOrDefault "SkipYarnInstall" "false" = "true"

let buildServer() =
  let serverSrc = root </> "server" </> "src"
  DotNetCli.Restore (fun p -> { p with WorkingDir = serverSrc })
  DotNetCli.Build (fun p -> { p with WorkingDir = serverSrc; Configuration = config; Output = serverBin })

let deployContent() =
    let content = sprintf "%s/Content" serverBin
    Fake.FileSystemHelper.ensureDirectory content

    !! "client/public/*.html"
    ++ "client/public/*.js"
    ++ "client/public/*.js.map"
    |> Seq.iter (Fake.FileHelper.CopyFileWithSubfolder "client/public" content)

Target "BuildServer" buildServer

Target "BuildMonitor" <| fun() ->
  DotNetCli.Restore (fun p -> { p with WorkingDir = root </> "monitor" })
  DotNetCli.Build (fun p -> { p with Project = root </> "monitor" </> "monitor.fsproj"; Configuration = config; Output = serverBin })

Target "BuildClient" <| fun _ ->
    if not skipYarnInstall then
        YarnHelper.Yarn (fun p -> {p with WorkingDirectory = root </> "client" })
    let clientSrc = root </> "client" </> "src"
    DotNetCli.Restore (fun p -> { p with WorkingDir = clientSrc })
    DotNetCli.RunCommand (fun p -> { p with WorkingDir = clientSrc }) "fable yarn-run build"

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
    async { YarnHelper.Yarn (fun p -> {p with Command = YarnCommand.Custom "run watch"; WorkingDirectory = root </> "client"}); return 0 }
    Shell.AsyncExec( "dotnet", args = (serverBin </> "Vapor.dll 8085"), dir = serverBin ) ]
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore

Target "CIBuild" DoNothing

"BuildClient" ==> "DeployContent" ==> "Build"
"BuildServer" ==> "Build"
"BuildMonitor" ==> "Build"
"Build" ==> "CIBuild"

RunTargetOrDefault "Build"
