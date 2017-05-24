open Printf
open Lwt
open Log
open Apputil_error_t

let report_error error_id error_msg =
  let t = Util_time.now () in
  let v = {
    error_id;
    error_date = t;
    error_hi_prio = Util_prio.is_high_priority_job ();
    error_service = Log.get_service ();
    error_msg;
  } in
  catch
    (fun () ->
       (* `unprotected_put`, unlike just `put`, avoids creating a lock,
          so as to not create a bottleneck when many identical errors
          are reported at the same time. *)
       Apputil_access.Uncaught.unprotected_put
         error_id (Util_rng.hex 8) v (Util_time.now ())
    )
    (fun e ->
       logf `Error "Error.report failed: %s" (string_of_exn e);
       return ()
    )

(*
   Report a freshly-caught exception.
   context_name can be anything like "api" or "daily agenda" or
   "async exception".
*)
let report_exn context_name e =
  let full_trace = Trax.to_string e in
  let error_id = Util_exn.trace_hash e in
  logf `Error "[%s] Error #%s: exception %s"
    context_name error_id full_trace;
  match Http_exn.classify (Trax.unwrap e) with
  | `Client_error _ ->
      (* Those would be client errors in an HTTP context and correspond
         normally to user accounts that have become invalid
         (access token was revoked, Google resource is gone, ...).
         We don't report them. *)
      return ()

  | `Server_error
  | `Other ->
      report_error error_id (context_name ^ ": \n" ^ full_trace)

(* Substitute for Lwt.catch *)
let catch_and_report context_name f g =
  catch f (fun e ->
    (match Trax.unwrap e with
     | Lwt.Canceled when Util_shutdown.is_shutting_down () -> return ()
     | e -> report_exn context_name e
    ) >>= fun () ->
    logf `Error "Reported exception: %s"
      (string_of_exn e);
    g e
  )

(* Report the error but don't let the exception propagate *)
let catch_report_ignore name f =
  catch_and_report name f (fun e ->
    return ()
  )

let read_latest max_age f =
  let min_ord = Util_time.(sub (now ()) max_age) in
  Apputil_access.Uncaught.iter ~min_ord (fun error_id random x t ->
    f x
  )

let format_service_counts l =
  String.concat ", " (
    BatList.map (fun (k, n) ->
      sprintf "%s: %i"
        (Util_html.encode k) n
    ) l
  )

let make_html_report l =
  match l with
  | [] ->
      Some ("No error today!", "<p>Hurray!</p>")
  | l ->
      let buf = Buffer.create 1000 in
      let subject = "Uncaught exceptions over the last 24 hours" in
      List.iter (fun (id, count, hi_prio_count, service_counts, example) ->
        bprintf buf "
<h2>Error #%s</h2>
<p>
  High-priority count: %i
  <br>
  Count: %i
  <br>
  Counts by service: %s
  <br>
  Example:
  <pre>%s\
  </pre>
</p>
"
          id
          hi_prio_count
          count
          (format_service_counts service_counts)
          (Util_html.encode example)
      ) l;
      Some (subject, Buffer.contents buf)

let create_counters () =
  Hashtbl.create 10

let incr_counter tbl k =
  let r =
    try Hashtbl.find tbl k
    with Not_found ->
      let r = ref 0 in
      Hashtbl.add tbl k r;
      r
  in
  incr r

let get_counts tbl =
  let l = Hashtbl.fold (fun k v acc -> (k, !v) :: acc) tbl [] in
  List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) l

let send_daily_aggregate () =
  let max_age = 86400. +. 60. in (* one day and some *)
  let tbl = Hashtbl.create 10 in
  read_latest max_age (fun x ->
    let counter, hi_prio_counter, service_counters, example =
      try Hashtbl.find tbl x.error_id
      with Not_found ->
        let example = x.error_msg in
        let counter = ref 0 in
        let hi_prio_counter = ref 0 in
        let service_counters = create_counters () in
        let v = (counter, hi_prio_counter, service_counters, example) in
        Hashtbl.add tbl x.error_id v;
        v
    in
    incr counter;
    let service = BatOption.default "other" x.error_service in
    incr_counter service_counters service;
    if x.error_hi_prio then
      incr hi_prio_counter;
    return ()
  ) >>= fun () ->
  let all =
    Hashtbl.fold (
      fun error_id (counter, hi_prio_counter,
                    service_counters, example) l ->
        let service_counts = get_counts service_counters in
        (error_id, !counter, !hi_prio_counter, service_counts, example) :: l
    ) tbl []
  in
  let all = List.sort (fun (_, n1, hi1, _, _) (_, n2, hi2, _, _) ->
    let c = compare hi2 hi1 in
    if c <> 0 then c
    else
      compare n2 n1
  ) all in
  match make_html_report all with
  | None -> return ()
  | Some (subject, html_body) ->
      let alerts_addr =
        let conf = Conf.get () in
        Email.of_string conf.Conf_t.developer_email
      in
      Email_esper.send_from_esper
        ~from:alerts_addr
        [alerts_addr]
        subject
        (`Html html_body)

(*
   Run a job with low priority or high priority.
   Non-HTTP exceptions are treated as internal server errors
   and are converted into the corresponding HTTP exception to avoid
   double-reporting.
*)
let with_priority is_high_priority f =
  Lwt.with_value Util_prio.key (Some is_high_priority)
    (fun () ->
       catch f
         (fun e ->
            let error_msg = Log.string_of_exn e in
            let error_id = Util_exn.trace_hash e in
            match Http_exn.classify (Trax.unwrap e) with
            | `Other ->
                let public_msg = sprintf "Internal error #%s" error_id in
                logf `Error "Internal error #%s: %s" error_id error_msg;
                report_error error_id error_msg >>= fun () ->
                Http_exn.internal_error public_msg
            | `Server_error
            | `Client_error _ ->
                Trax.raise __LOC__ e
         )
    )

let main ~offset =
  Cmdline.parse_options ~offset [];
  if Esper_config.is_prod () then
    Util_lwt_main.run (send_daily_aggregate ())
