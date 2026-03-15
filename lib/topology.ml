(* Topology configuration — peer addresses and connection policy.

   Parses both legacy and P2P topology file formats. *)

type topology_entry = {
  te_host : string;
  te_port : int;
  te_valency : int;
  te_advertise : bool;
  te_is_root : bool;
}

type topology = {
  local_roots : topology_entry list;
  public_roots : topology_entry list;
}

let parse_access_points ?(advertise = false) ?(is_root = true) json =
  match Json.get "accessPoints" json with
  | Some (Json.Array items) ->
    let valency = match Json.get "valency" json with
      | Some v -> (match Json.to_int v with Some n -> n | None -> 1)
      | None -> 1 in
    let adv = match Json.get "advertise" json with
      | Some v -> (match Json.to_bool v with Some b -> b | None -> advertise)
      | None -> advertise in
    List.filter_map (fun item ->
      let host = match Json.get "address" item with
        | Some v -> (match Json.to_string v with Some s -> s | None -> "")
        | None -> "" in
      let port = match Json.get "port" item with
        | Some v -> (match Json.to_int v with Some n -> n | None -> 3001)
        | None -> 3001 in
      if host <> "" then
        Some { te_host = host; te_port = port; te_valency = valency;
               te_advertise = adv; te_is_root = is_root }
      else None
    ) items
  | _ -> []

let parse_topology ~path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let st = Unix.fstat fd in
  let buf = Bytes.create st.Unix.st_size in
  let rec go off rem =
    if rem = 0 then () else let n = Unix.read fd buf off rem in go (off+n) (rem-n)
  in go 0 st.st_size;
  Unix.close fd;
  let json = Json.parse (Bytes.to_string buf) in
  (* Try P2P format first *)
  let local_roots = match Json.get "localRoots" json with
    | Some (Json.Array groups) ->
      List.concat_map (parse_access_points ~is_root:true) groups
    | _ -> [] in
  let public_roots = match Json.get "publicRoots" json with
    | Some (Json.Array groups) ->
      List.concat_map (parse_access_points ~is_root:false) groups
    | _ -> [] in
  if local_roots <> [] || public_roots <> [] then
    Ok { local_roots; public_roots }
  else
    (* Try legacy format *)
    match Json.get "Producers" json with
    | Some (Json.Array items) ->
      let entries = List.filter_map (fun item ->
        let host = match Json.get "addr" item with
          | Some v -> (match Json.to_string v with Some s -> s | None -> "")
          | None -> "" in
        let port = match Json.get "port" item with
          | Some v -> (match Json.to_int v with Some n -> n | None -> 3001)
          | None -> 3001 in
        let valency = match Json.get "valency" item with
          | Some v -> (match Json.to_int v with Some n -> n | None -> 1)
          | None -> 1 in
        if host <> "" then
          Some { te_host = host; te_port = port; te_valency = valency;
                 te_advertise = false; te_is_root = true }
        else None
      ) items in
      Ok { local_roots = entries; public_roots = [] }
    | _ -> Error "unrecognized topology format"

let rec default_topology = function
  | "preview" ->
    { local_roots = [{ te_host = "preview-node.play.dev.cardano.org"; te_port = 3001;
                       te_valency = 1; te_advertise = false; te_is_root = true }];
      public_roots = [] }
  | "preprod" ->
    { local_roots = [{ te_host = "preprod-node.play.dev.cardano.org"; te_port = 3001;
                       te_valency = 1; te_advertise = false; te_is_root = true }];
      public_roots = [] }
  | "mainnet" ->
    { local_roots = [
        { te_host = "backbone.cardano.iog.io"; te_port = 3001;
          te_valency = 2; te_advertise = false; te_is_root = true };
        { te_host = "backbone.mainnet.emurgo.me"; te_port = 3001;
          te_valency = 1; te_advertise = false; te_is_root = true };
        { te_host = "relays-new.cardano-mainnet.iohk.io"; te_port = 3001;
          te_valency = 2; te_advertise = false; te_is_root = true }];
      public_roots = [] }
  | _ -> default_topology "preview"

let all_entries topo =
  topo.local_roots @ topo.public_roots
