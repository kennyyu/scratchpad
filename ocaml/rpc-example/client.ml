open Core.Std
open Async.Std
open Async_extra.Std
open Protocol

let add connection query =
  Rpc.Rpc.dispatch Add.rpc connection query
  >>= fun response_or_err ->
  let response = Or_error.ok_exn response_or_err in
  Add.sexp_of_response response |! Sexp.to_string |! printf "%s\n";
  return ()

let product connection query =
  Rpc.Rpc.dispatch Product.rpc connection query
  >>= fun response_or_err ->
  let response = Or_error.ok_exn response_or_err in
  Product.sexp_of_response response |! Sexp.to_string |! printf "%s\n";
  return ()

let start host port f =
  Rpc.Connection.with_client ~host ~port f
  >>| fun result -> Result.ok_exn result

let command_rpc (f, summary) =
  Command.basic
    ~summary
    Command.Spec.(
      empty
      +> flag "-host" (required string) ~doc:" Host to dispatch"
      +> flag "-port" (required int) ~doc:" Port to dispatch"
      +> flag "-x" (required int) ~doc:" First operand"
      +> flag "-y" (required int) ~doc:" Second operand"
    )
    (fun host port x y () ->
      don't_wait_for (
        start host port (f x y)
        >>| fun () -> shutdown 0
      )
    )

let command =
  Command.group
    ~summary:"Calculator Client"
    [ ("add",command_rpc
      ((fun x y -> fun con -> add con { Add.x = x; y = y }),
       "Add RPC"))
    ; ("product", command_rpc
      ((fun x y -> fun con -> product con { Product.x = x; y = y }),
       "Product RPC"))
    ]

let () =
  Command.run command;
  never_returns (Scheduler.go ())
