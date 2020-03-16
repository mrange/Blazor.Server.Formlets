namespace Blazor.Formlet

module Log =
  open FSharp.Core.Printf
  open System

  type [<RequireQualifiedAccess>] LogLevel = 
    | Success   = 5
    | Hilight   = 4
    | Info      = 3
    | Warning   = 2
    | Error     = 1
  let log   level msg = 
    let cc =
      match level with
      | LogLevel.Success  -> ConsoleColor.Green
      | LogLevel.Hilight  -> ConsoleColor.Cyan
      | LogLevel.Info     -> ConsoleColor.Gray
      | LogLevel.Warning  -> ConsoleColor.Yellow
      | LogLevel.Error    
      | _                 -> ConsoleColor.Red
    let pre =
      match level with
      | LogLevel.Success  -> "SUCCESS: "
      | LogLevel.Hilight  -> "HILIGHT: "
      | LogLevel.Info     -> "INFO   : "
      | LogLevel.Warning  -> "WARNING: "
      | LogLevel.Error    -> "ERROR  : "
      | _                 -> "UNKNOWN: "
    let occ = Console.ForegroundColor
    Console.ForegroundColor <- cc
    try
      Console.WriteLine (pre + msg)
    finally
      Console.ForegroundColor <- occ

  let logf  level fmt = kprintf (log level) fmt

  let success msg = log LogLevel.Success msg
  let hilight msg = log LogLevel.Hilight msg
  let info    msg = log LogLevel.Info    msg
  let warning msg = log LogLevel.Warning msg
  let error   msg = log LogLevel.Error   msg

  let successf fmt = kprintf success fmt
  let hilightf fmt = kprintf hilight fmt
  let infof    fmt = kprintf info    fmt
  let warningf fmt = kprintf warning fmt
  let errorf   fmt = kprintf error   fmt


