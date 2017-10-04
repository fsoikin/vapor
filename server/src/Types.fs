module Vapor.Types

type ProcessName = string

type Pid = int

type ProcessState = Stopped | Stopping | Running of Pid

type Process = { Name: ProcessName; Cmd: string; State: ProcessState }

let timeFormat = "yyyy-MM-dd HH:mm:ss.fff"

let minTime = System.DateTime(2000,01,01)
