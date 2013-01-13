open Core.Std
open Async_extra.Std

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


