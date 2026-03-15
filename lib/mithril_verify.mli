(** Mithril certificate chain verification. *)

val verify_certificate_hash :
  Mithril_client.certificate -> (unit, string) result

val verify_certificate_chain :
  ?aggregator:string -> leaf_hash:string -> unit ->
  (int, string) result
(** Returns Ok chain_length or Error. *)

val compute_file_digest : path:string -> (string, string) result
val verify_snapshot_digest :
  expected_digest:string -> snapshot_path:string -> (unit, string) result
