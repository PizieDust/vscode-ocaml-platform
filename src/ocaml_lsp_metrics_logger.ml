open Import

module Msg = struct
  (* We use an intermediate object to wrap a log message, VScode prohibes logging just a string.
     See https://code.visualstudio.com/api/references/vscode-api#TelemetrySender. *)
  let from_string msg = Ojs.obj [| "msg", Ojs.string_to_js msg |]
  let to_string obj = Ojs.get_prop obj (Ojs.string_to_js "msg") |> Ojs.string_of_js
end

let log_line ~prefix event_name msg_data =
  let msg = Option.fold msg_data ~init:"empty" ~f:(fun _ -> Msg.to_string) in
  prefix ^ "\n" ^ event_name ^ ": " ^ msg
;;

let on_log ~fs ~uri event_name data =
  (* TODO: handle properly FS errors? *)
  (* if FileSystem.isWritableFileSystem fs ~scheme:"file"
     then *)
  (* Make sure the file exists by writing an empty string in it. *)
  (* let _ = FileSystem.writeFile fs ~uri:log_file_path "" in *)
  let open Promise.Syntax in
  let* content = FileSystem.readFile fs ~uri in
  log "reading: %s" content;
  let+ () = FileSystem.writeFile fs ~uri (log_line ~prefix:content event_name data) in
  log "writing";
  ()
;;

let create_logger () =
  let fs = Workspace.fs () in
  let log_file_path =
    Path.(Option.value_exn (Sandbox.workspace_root ()) / "ocaml-lsp-metrics.logs")
    |> Path.to_string
  in
  let sender =
    TelemetrySender.create
      ~sendErrorData:(fun ~error:_ ~data:_ -> ())
      ~sendEventData:(fun ~eventName ~data ->
        let _ = on_log ~fs ~uri:log_file_path eventName data in
        ())
  in
  Env.createTelemetryLogger ~sender ()
;;

let log logger ~name msg =
  TelemetryLogger.logUsage logger ~eventName:name ~data:(Msg.from_string msg)
;;

let register logger extension =
  let disposable = Disposable.make ~dispose:(fun () -> TelemetryLogger.dispose logger) in
  ExtensionContext.subscribe extension ~disposable
;;
