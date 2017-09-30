module Vapor.Types

type Process = { Name: string; Cmd: string; Pid: int option }

let timeFormat = "yyyy-MM-dd HH:mm:ss.ff"

let minTime = System.DateTime(2000,01,01)
