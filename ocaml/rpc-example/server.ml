open Core.Std
open Async.Std
open Async_extra.Std
open Protocol

let add = Rpc.Rpc.implement
  Add.rpc (fun () query ->
    Add.sexp_of_query query |! Sexp.to_string |! printf "%s\n";
    let sum = query.Add.x + query.Add.y in
    return { Add.sum = sum })

let product = Rpc.Rpc.implement
  Product.rpc (fun () query ->
    Product.sexp_of_query query |! Sexp.to_string |! printf "%s\n";
    let product = query.Product.x * query.Product.y in
    return { Product.product = product })

let start port =
  let implementations = Rpc.Implementations.create_exn (* Fail quickly *)
    ~implementations:[add; product]
    ~on_unknown_rpc:`Raise
  in
  don't_wait_for (
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _socket_state -> ())
      ~where_to_listen:(Tcp.on_port port)
      ()
    >>= fun server -> Tcp.Server.close_finished server
  )

let command =
  Command.basic
    ~summary:"Start Calculator Server"
    Command.Spec.(
      empty
      +> flag "-port" (required int) ~doc:" Port to listen"
    )
    (fun port () -> start port)

let () =
  Command.run command;
  never_returns (Scheduler.go ())
