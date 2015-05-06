(*
   Let other modules register handlers.
   This is used to solve mutual module dependencies and it should
   not be abused.
*)

open Lwt
open Log

type 'arg t = {
  register: string -> ('arg -> unit Lwt.t) -> unit;
  run: 'arg -> unit Lwt.t;
}

let create hook_name : 'a t =
  let handlers = ref [] in
  let register_handler handler_name f =
    handlers := (hook_name ^ "." ^ handler_name, f) :: !handlers
  in
  let run_handler name f arg =
    Apputil_error.catch_report_ignore name (fun () -> f arg)
  in
  let run_all_handlers arg =
    Lwt_list.iter_s (fun (name, f) -> run_handler name f arg) !handlers
  in
  { register = register_handler;
    run = run_all_handlers }
