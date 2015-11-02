open Lwt
open Log

(*
   Read a process ID from a file.
   If the file exists, it must contain just an int.
*)
let get_previous_worker_pid pid_file =
  if Sys.file_exists pid_file then
    let pid_string = BatPervasives.input_file pid_file in
    Some (int_of_string pid_string)
  else
    None

let save_pid pid_file =
  BatPervasives.output_file
    ~filename: pid_file
    ~text: (string_of_int (Unix.getpid()))

let process_is_killable pid =
  try
    Unix.kill pid 0;
    true
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) ->
      (*
         The pid or process group does not exist. Note that an existing
         process might be a zombie, a process which already committed
         termination, but has not yet been wait(2)ed for.
      *)
      logf `Info "Process %i does not exist" pid;
      false

  | Unix.Unix_error (error, _, _) ->
      logf `Info "Process %i is not killable: %s"
        pid (Unix.error_message error);
      false

let previous_worker_is_still_running pid_file =
  Apputil_error.catch_and_report
    "Apputil_worker.previous_worker_is_still_running"
    (fun () ->
       match get_previous_worker_pid pid_file with
       | None -> return false
       | Some pid -> return (process_is_killable pid)
    )
    (fun e -> return false)
