open Core.Std
open Async.Std
open Async_extra.Std

(** Definitions of the available RPC's. *)
module Calculator = struct

  module Add = struct
    type query = { x : int; y : int }
    with sexp, bin_io

    type response = { sum : int }
    with sexp, bin_io

    let rpc = Rpc.Rpc.create
      ~name:"Add"
      ~version:0
      ~bin_query
      ~bin_response
  end

  module Product = struct
    type query = { x : int; y : int }
    with sexp, bin_io

    type response = { product : int }
    with sexp, bin_io

    let rpc = Rpc.Rpc.create
      ~name:"Product"
      ~version:0
      ~bin_query
      ~bin_response
  end

end

(** Server provides implementations of the protocols. *)
module Server = struct
  open Calculator
  exception Bad

  let add =
    Rpc.Rpc.implement
      Add.rpc
      (fun () query ->
        printf "%s\n" (Sexp.to_string (Add.sexp_of_query query));
        let sum = query.Add.x + query.Add.y in
        return { Add.sum = sum })

  let product =
    Rpc.Rpc.implement
      Calculator.Product.rpc
      (fun () query ->
        printf "%s\n" (Sexp.to_string (Product.sexp_of_query query));
        let product = query.Product.x * query.Product.y in
        return { Product.product = product })

  let start port =
    Rpc.Implementations.create
      ~implementations:[add; product]
      ~on_unknown_rpc:`Raise
    |! function
        | Result.Ok implementations ->
          don't_wait_for (
            Rpc.Connection.serve
              ~implementations
              ~initial_connection_state:(fun _ -> ())
              ~where_to_listen:(Tcp.on_port port)
              ()
            >>= fun _ -> return ()
          )
        | Result.Error _ -> raise Bad (* TODO: handle this better *)

  let command =
    Command.basic
      ~summary:"Start Calculator Server"
      Command.Spec.(
        empty
        +> flag "-port" (required int) ~doc:"Port to listen"
      )
      (fun port () -> start port)
end

(** Client dispatches RPC requests to server. *)
module Client = struct
  open Calculator

  let add connection query =
    Rpc.Rpc.dispatch Add.rpc connection query
    >>= fun response_or_err ->
    let response = Or_error.ok_exn response_or_err in
    printf "%s\n" (Sexp.to_string (Add.sexp_of_response response));
    return ()

  let product connection query =
    Rpc.Rpc.dispatch Product.rpc connection query
    >>= fun response_or_err ->
    let response = Or_error.ok_exn response_or_err in
    printf "%s\n" (Sexp.to_string (Product.sexp_of_response response));
    return ()

  let start host port =
    Rpc.Connection.client ~host ~port
    >>| fun result -> Result.ok_exn result

  let command f =
    Command.basic
      ~summary:"Start Calculator Client"
      Command.Spec.(
        empty
        +> flag "-host" (required string) ~doc:"Host to dispatch"
        +> flag "-port" (required int) ~doc:"Port to dispatch"
        +> flag "-x" (required int) ~doc:"First operand"
        +> flag "-y" (required int) ~doc:"Second operand"
      )
      (fun host port x y () ->
        don't_wait_for (
          start host port
          >>= fun connection -> f connection x y
          >>| fun () -> shutdown 0
        )
      )
end

let command =
  let open Calculator in
  Command.group
    ~summary:"Calculator RPC example"
    [ ("server", Server.command)
    ; ("client-add", Client.command
      (fun con x y -> Client.add con { Add.x = x; y = y} ))
    ; ("client-product", Client.command
      (fun con x y -> Client.product con { Product.x = x; y = y} ))
    ]

(** Run our command line arguments and start the Async scheduler. *)
let () =
  Command.run command;
  never_returns (Scheduler.go ())
