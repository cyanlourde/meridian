(* TCP connection management for Ouroboros networking.

   Provides a thin wrapper around Unix sockets with timeout handling,
   clean shutdown, and error reporting. Used by the network layer to
   establish connections to Cardano nodes. *)

type t = {
  fd : Unix.file_descr;
  remote_addr : string;
  remote_port : int;
}

type connection_error =
  | Dns_error of string
  | Connection_refused of string
  | Timeout
  | Network_error of string

let error_to_string = function
  | Dns_error msg -> Printf.sprintf "DNS error: %s" msg
  | Connection_refused msg -> Printf.sprintf "connection refused: %s" msg
  | Timeout -> "connection timed out"
  | Network_error msg -> Printf.sprintf "network error: %s" msg

(** Connect to a remote host:port with an optional timeout in seconds.
    Returns a connection handle or an error. *)
let connect ?(timeout_s = 30.0) ~host ~port () =
  (* Resolve hostname *)
  let addrs =
    try
      let entry = Unix.gethostbyname host in
      if Array.length entry.Unix.h_addr_list = 0 then
        Error (Dns_error (Printf.sprintf "no addresses for %s" host))
      else
        Ok entry.Unix.h_addr_list.(0)
    with
    | Not_found ->
      Error (Dns_error (Printf.sprintf "host not found: %s" host))
    | Unix.Unix_error (e, _, _) ->
      Error (Dns_error (Unix.error_message e))
  in
  match addrs with
  | Error _ as e -> e
  | Ok addr ->
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (* Set non-blocking for connect timeout *)
    Unix.set_nonblock fd;
    let sockaddr = Unix.ADDR_INET (addr, port) in
    let connected =
      try
        Unix.connect fd sockaddr;
        Ok ()  (* immediate connect, rare *)
      with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
        (* Wait for connect to complete using select *)
        let _, wfds, _ = Unix.select [] [fd] [] timeout_s in
        if wfds = [] then begin
          Unix.close fd;
          Error Timeout
        end else begin
          (* Check for connection error via getsockopt *)
          let err = Unix.getsockopt_int fd Unix.SO_ERROR in
          if err <> 0 then begin
            Unix.close fd;
            Error (Connection_refused (Printf.sprintf "SO_ERROR=%d" err))
          end else
            Ok ()
        end
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
        Unix.close fd;
        Error (Connection_refused (Printf.sprintf "%s:%d" host port))
      | Unix.Unix_error (e, _, _) ->
        Unix.close fd;
        Error (Network_error (Unix.error_message e))
    in
    match connected with
    | Error _ as e -> e
    | Ok () ->
      (* Switch back to blocking mode for normal I/O *)
      Unix.clear_nonblock fd;
      Ok { fd; remote_addr = host; remote_port = port }

(** Send all bytes over the connection. *)
let send_bytes conn data =
  let len = Bytes.length data in
  let rec go off remaining =
    if remaining = 0 then Ok ()
    else
      try
        let n = Unix.write conn.fd data off remaining in
        if n = 0 then Error (Network_error "write returned 0")
        else go (off + n) (remaining - n)
      with
      | Unix.Unix_error (e, _, _) ->
        Error (Network_error (Unix.error_message e))
  in
  go 0 len

(** Receive exactly [n] bytes from the connection. *)
let recv_bytes conn n =
  let buf = Bytes.create n in
  let rec go off remaining =
    if remaining = 0 then Ok buf
    else
      try
        let k = Unix.read conn.fd buf off remaining in
        if k = 0 then Error (Network_error "connection closed")
        else go (off + k) (remaining - k)
      with
      | Unix.Unix_error (e, _, _) ->
        Error (Network_error (Unix.error_message e))
  in
  go 0 n

(** Get the underlying file descriptor (for use with Mux). *)
let file_descr conn = conn.fd

(** Close the connection. *)
let close conn =
  (try Unix.shutdown conn.fd Unix.SHUTDOWN_ALL with _ -> ());
  (try Unix.close conn.fd with _ -> ())

(** String representation for logging. *)
let to_string conn =
  Printf.sprintf "%s:%d" conn.remote_addr conn.remote_port
