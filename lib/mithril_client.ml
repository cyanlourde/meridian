(* Mithril aggregator API client.

   Talks to the Mithril aggregator REST API to list, download, and
   verify snapshots. Uses curl for HTTP/TLS/redirect handling.

   Reference: https://mithril.network/doc/ *)

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

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

let preview_aggregator =
  "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
let preprod_aggregator =
  "https://aggregator.release-preprod.api.mithril.network/aggregator"
let mainnet_aggregator =
  "https://aggregator.release-mainnet.api.mithril.network/aggregator"

let network_aggregator = function
  | "preview" -> preview_aggregator
  | "preprod" -> preprod_aggregator
  | "mainnet" -> mainnet_aggregator
  | _ -> preview_aggregator

(* ================================================================ *)
(* HTTP via curl                                                     *)
(* ================================================================ *)

(** Run curl and capture stdout. Returns Ok body or Error msg. *)
let curl_get url =
  let tmp = Filename.temp_file "meridian-curl" ".json" in
  let cmd = Printf.sprintf "curl -sL -o %s -w '%%{http_code}' '%s' 2>/dev/null"
    (Filename.quote tmp) url in
  let ic = Unix.open_process_in cmd in
  let code = try input_line ic with End_of_file -> "000" in
  ignore (Unix.close_process_in ic);
  if String.length code >= 3 && code.[0] = '2' then begin
    let fd = Unix.openfile tmp [Unix.O_RDONLY] 0 in
    let st = Unix.fstat fd in
    let buf = Bytes.create st.Unix.st_size in
    let rec go off rem =
      if rem = 0 then () else let n = Unix.read fd buf off rem in go (off+n) (rem-n)
    in go 0 st.st_size;
    Unix.close fd;
    Unix.unlink tmp;
    Ok (Bytes.to_string buf)
  end else begin
    (try Unix.unlink tmp with _ -> ());
    Error (Printf.sprintf "HTTP %s from %s" code url)
  end

(** Download a file with progress. Returns Ok () or Error. *)
let curl_download ~url ~dest =
  let cmd = Printf.sprintf "curl -L --progress-bar -o %s '%s'"
    (Filename.quote dest) url in
  let ret = Sys.command cmd in
  if ret = 0 then Ok ()
  else Error (Printf.sprintf "curl download failed (exit %d)" ret)

(* ================================================================ *)
(* JSON response parsing                                             *)
(* ================================================================ *)

let parse_snapshot_summary json =
  let digest = match Json.get "digest" json with
    | Some v -> (match Json.to_string v with Some s -> s | None -> "")
    | None -> "" in
  let beacon = match Json.get "beacon" json with Some b -> b | None -> Json.Null in
  let beacon_epoch = match Json.get "epoch" beacon with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> 0L)
    | None -> 0L in
  let beacon_immutable = match Json.get "immutable_file_number" beacon with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> 0L)
    | None -> 0L in
  let size = match Json.get "size" json with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> 0L)
    | None -> 0L in
  let created_at = match Json.get "created_at" json with
    | Some v -> (match Json.to_string v with Some s -> s | None -> "")
    | None -> "" in
  let locations = match Json.get "locations" json with
    | Some (Json.Array items) ->
      List.filter_map (fun item -> Json.to_string item) items
    | _ -> [] in
  { digest; beacon_epoch; beacon_immutable; size; created_at; locations }

let parse_certificate json =
  let str key = match Json.get key json with
    | Some v -> (match Json.to_string v with Some s -> s | None -> "")
    | None -> "" in
  let beacon = match Json.get "beacon" json with Some b -> b | None -> Json.Null in
  let epoch = match Json.get "epoch" beacon with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> 0L)
    | None -> 0L in
  let immut = match Json.get "immutable_file_number" beacon with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> 0L)
    | None -> 0L in
  { cert_hash = str "hash";
    previous_hash = str "previous_hash";
    beacon_epoch = epoch;
    beacon_immutable = immut;
    signed_message = str "signed_message";
    aggregate_verification_key = str "aggregate_verification_key" }

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

let list_snapshots ?(aggregator = preview_aggregator) () =
  let url = aggregator ^ "/artifact/snapshots" in
  match curl_get url with
  | Error e -> Error e
  | Ok body ->
    let json = Json.parse body in
    match json with
    | Json.Array items ->
      Ok (List.map parse_snapshot_summary items)
    | _ -> Error "expected JSON array of snapshots"

let get_certificate ?(aggregator = preview_aggregator) ~hash () =
  let url = aggregator ^ "/certificate/" ^ hash in
  match curl_get url with
  | Error e -> Error e
  | Ok body ->
    let json = Json.parse body in
    Ok (parse_certificate json)

let download_snapshot ~url ~dest =
  curl_download ~url ~dest
