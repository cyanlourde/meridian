(* Unix domain socket listener for local client connections.

   Creates a Unix socket, accepts connections, and spawns a handler
   for each client with its own mux in Responder mode. *)

type t = {
  socket_path : string;
  fd : Unix.file_descr;
  mutable running : bool;
}

(** Create and bind a Unix domain socket. Removes stale socket file. *)
let create ~socket_path =
  (if Sys.file_exists socket_path then
     try Unix.unlink socket_path with _ -> ());
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX socket_path);
  Unix.chmod socket_path 0o660;
  Unix.listen fd 5;
  Unix.set_nonblock fd;
  { socket_path; fd; running = true }

(** Accept one client connection. Returns the client file descriptor
    or None if the listener was stopped. *)
let accept t =
  if not t.running then None
  else
    try
      let (client_fd, _addr) = Unix.accept t.fd in
      Some client_fd
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> None
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> None
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> None
    | Unix.Unix_error _ -> None

(** Stop the listener and clean up the socket file. *)
let close t =
  t.running <- false;
  (try Unix.close t.fd with _ -> ());
  (try Unix.unlink t.socket_path with _ -> ())

let socket_path t = t.socket_path
let is_running t = t.running
