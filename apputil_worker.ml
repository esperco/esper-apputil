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

let kill ~pid ~signal =
  try
    Unix.kill pid signal;
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
      logf `Info "Process %i is not killable with signal %i: %s"
        pid signal (Unix.error_message error);
      false

(*
   Return whether the process can be killed, i.e. roughly whether it exists.
*)
let process_is_killable pid =
  kill ~pid ~signal:0


(*
   Determine the age of a process so we can kill it if it's gotten too old
   when it shouldn't.
   This works on Linux and returns None on MacOS.
*)
let linux_get_process_age pid =
  let open Unix in
  try
    let creation_date = (stat ("/proc/" ^ string_of_int pid)).st_ctime in
    let dt = Unix.gettimeofday () -. creation_date in
    if dt >= 0. then Some dt
    else None
  with _ ->
    None

(*
   Return (Some true) if process was killed, (Some false) if process
   couldn't or shouldn't be killed, and None if we can't determine its age.
*)
let linux_kill_if_older_than pid max_age =
  match linux_get_process_age pid with
  | None -> None
  | Some age ->
      if age >= max_age then (
        logf `Error "Found very old process %i. Killing it." pid;
        Some (kill ~pid ~signal:Sys.sigkill)
      )
      else
        Some false

let previous_worker_is_still_running ?max_age pid_file =
  Apputil_error.catch_and_report
    "Apputil_worker.previous_worker_is_still_running"
    (fun () ->
       match get_previous_worker_pid pid_file with
       | None -> return false
       | Some pid ->
           match process_is_killable pid with
           | true ->
               (match max_age with
                | Some max_age ->
                    (match linux_kill_if_older_than pid max_age with
                     | None ->
                         (* should not happen on Linux *)
                         assert (not (Esper_config.is_prod ()));
                         return true
                     | Some true ->
                         (* killed, no longer running *)
                         return false
                     | Some false ->
                         (* not killed, still running *)
                         return true
                    )
                | None ->
                    return true
               )
           | false ->
               return false
    )
    (fun e -> return false)
