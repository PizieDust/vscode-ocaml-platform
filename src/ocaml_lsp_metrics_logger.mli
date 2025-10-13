val create_logger : unit -> Vscode.TelemetryLogger.t
val log : Vscode.TelemetryLogger.t -> name:string -> string -> unit
val register : Vscode.TelemetryLogger.t -> Vscode.ExtensionContext.t -> unit
