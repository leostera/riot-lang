(** gRPC Protocol Implementation

    This package provides protocol-level types and utilities for gRPC.
    It does NOT provide client/server implementations - those are in:
    - blink: Blink.GRPC for gRPC clients
    - suri: Suri.GRPC for gRPC servers

    Usage:
    {[
      (* Define a method *)
      let get_user = Grpc.Call.unary_method
        ~service:"example.UserService"
        ~method_:"GetUser"

      (* Create metadata *)
      let metadata = Grpc.Metadata.empty
        |> Grpc.Metadata.add ~key:"authorization" ~value:"Bearer token123"

      (* Encode a request message *)
      let request_bytes = (* ... protobuf encoding ... *) in
      let framed = Grpc.Message.encode ~compressed:false ~payload:request_bytes

      (* Parse status from trailers *)
      match Grpc.Metadata.parse_status "0" with
      | Some Grpc.Status.OK -> (* success *)
      | _ -> (* error *)
    ]}
*)

(** {1 Modules} *)

(** Status codes *)
module Status = Status

(** Message framing (5-byte header + payload) *)
module Message = Message

(** Message reader for parsing gRPC frames *)
module Message_reader = Message_reader

(** Metadata (HTTP/2 headers) *)
module Metadata = Metadata

(** Call types and configuration *)
module Call = Call

(** Code generation from protobuf services *)
module Codegen = Codegen
