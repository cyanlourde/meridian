(* TCP listener for inbound peer connections. *)

type t = {
  fd : Unix.file_descr;
  port : int;
  mutable running : bool;
}

let create ~port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen fd 10;
  Unix.set_nonblock fd;
  { fd; port; running = true }

let accept t =
  if not t.running then None
  else
    try
      let (client_fd, addr) = Unix.accept t.fd in
      let remote = match addr with
        | Unix.ADDR_INET (a, p) ->
          Printf.sprintf "%s:%d" (Unix.string_of_inet_addr a) p
        | _ -> "unknown" in
      Some (client_fd, remote)
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
    | Unix.Unix_error (Unix.EINTR, _, _) -> None
    | Unix.Unix_error _ -> None

let close t =
  t.running <- false;
  (try Unix.close t.fd with _ -> ())

let port t = t.port
let is_running t = t.running
