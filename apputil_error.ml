open Printf
open Lwt
open Log
open Apputil_error_t

let report error_id error_msg =
  let t = Util_time.now () in
  let v = {
    error_id;
    error_date = t;
    error_msg;
  } in
  catch
    (fun () ->
       Apputil_access.Uncaught.put error_id (Util_rng.hex 8) v
    )
    (fun e ->
       logf `Error "Error.report failed: %s" (string_of_exn e);
       return ()
    )

(* Substitute for Lwt.catch *)
let catch_and_report name f g =
  catch f (fun e ->
    let full_trace = Util_exn.string_of_exn e in
    let error_id = Util_exn.trace_hash e in
    logf `Error "Error #%s in %s: exception %s"
      error_id name full_trace;
    report error_id (name ^ ": \n" ^ full_trace) >>= fun () ->
    g e
  )

(* Report the error but don't let the exception propagate *)
let catch_report_ignore name f =
  catch_and_report name f (fun e -> return ())

let read_latest max_age f =
  let min_ord = Util_time.(sub (now ()) max_age) in
  Apputil_access.Uncaught.iter ~min_ord (fun (error_id, random, x, t) ->
    f x
  )

let make_html_report l =
  match l with
  | [] -> None
  | l ->
      let buf = Buffer.create 1000 in
      let subject = "Uncaught exceptions over the last 24 hours" in
      List.iter (fun (id, count, example) ->
        bprintf buf "
<h2>Error #%s</h2>
<p>
  Count: %i
  <br>
  Example:
  <pre>%s\
  </pre>
</p>
"
          id count (Util_html.encode example)
      ) l;
      Some (subject, Buffer.contents buf)

let send_daily_aggregate () =
  let max_age = 86400. +. 60. in (* one day and some *)
  let tbl = Hashtbl.create 10 in
  read_latest max_age (fun x ->
    let counter =
      try fst (Hashtbl.find tbl x.error_id)
      with Not_found ->
        let example = x.error_msg in
        let counter = ref 0 in
        Hashtbl.add tbl x.error_id (counter, example);
        counter
    in
    incr counter;
    return ()
  ) >>= fun () ->
  let all = Hashtbl.fold (fun error_id (counter, example) l ->
    (error_id, !counter, example) :: l
  ) tbl []
  in
  let all = List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) all in
  match make_html_report all with
  | None -> return ()
  | Some (subject, html_body) ->
      let alerts_addr =
        let conf = Config.get () in
        Email.of_string conf.Config_t.developer_email
      in
      Email_esper.send_from_esper
        ~from: (`External (None, alerts_addr))
        [alerts_addr]
        subject
        html_body

let main ~offset =
  Cmdline.parse_options ~offset [];
  if Esper_config.is_prod () then
    Util_lwt_main.run (send_daily_aggregate ())
