(** Mithril aggregator API client. *)

type snapshot_summary = {
  digest : string;
  beacon_epoch : int64;
  beacon_immutable : int64;
  size : int64;
  created_at : string;
  locations : string list;
}

type certificate = {
  cert_hash : string;
  previous_hash : string;
  beacon_epoch : int64;
  beacon_immutable : int64;
  signed_message : string;
  aggregate_verification_key : string;
}

val preview_aggregator : string
val preprod_aggregator : string
val mainnet_aggregator : string
val network_aggregator : string -> string

val list_snapshots : ?aggregator:string -> unit ->
  (snapshot_summary list, string) result

val get_certificate : ?aggregator:string -> hash:string -> unit ->
  (certificate, string) result

val download_snapshot : url:string -> dest:string ->
  (unit, string) result

val parse_snapshot_summary : Json.t -> snapshot_summary
val parse_certificate : Json.t -> certificate
