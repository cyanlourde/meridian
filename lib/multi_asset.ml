(* Multi-asset value type for Cardano (Mary+ eras).

   CDDL: value = coin / [coin, multiasset<uint>]
          multiasset<a> = { * policy_id => { * asset_name => a } }
          policy_id = scripthash (28 bytes)
          asset_name = bytes .size (0..32) *)

type policy_id = bytes   (* 28-byte script hash *)
type asset_name = bytes  (* 0-32 bytes *)

(** An asset bundle: policy_id -> [(asset_name, quantity)] *)
type assets = (policy_id * (asset_name * int64) list) list

type value = {
  lovelace : int64;
  assets : assets;
}

let zero = { lovelace = 0L; assets = [] }

let of_lovelace n = { lovelace = n; assets = [] }

let lovelace_of v = v.lovelace

let asset_count v =
  List.fold_left (fun acc (_, alist) -> acc + List.length alist) 0 v.assets

let is_lovelace_only v = v.assets = []

(* ================================================================ *)
(* Asset map operations                                              *)
(* ================================================================ *)

(** Merge two asset name maps, summing quantities. *)
let merge_name_map a b =
  let tbl = Hashtbl.create (List.length a + List.length b) in
  List.iter (fun (name, qty) ->
    let cur = try Hashtbl.find tbl name with Not_found -> 0L in
    Hashtbl.replace tbl name (Int64.add cur qty)
  ) a;
  List.iter (fun (name, qty) ->
    let cur = try Hashtbl.find tbl name with Not_found -> 0L in
    Hashtbl.replace tbl name (Int64.add cur qty)
  ) b;
  Hashtbl.fold (fun name qty acc -> (name, qty) :: acc) tbl []

(** Merge two asset maps (policy level), summing quantities. *)
let merge_assets a b =
  let tbl = Hashtbl.create (List.length a + List.length b) in
  List.iter (fun (pid, names) ->
    let cur = try Hashtbl.find tbl pid with Not_found -> [] in
    Hashtbl.replace tbl pid (merge_name_map cur names)
  ) a;
  List.iter (fun (pid, names) ->
    let cur = try Hashtbl.find tbl pid with Not_found -> [] in
    Hashtbl.replace tbl pid (merge_name_map cur names)
  ) b;
  Hashtbl.fold (fun pid names acc -> (pid, names) :: acc) tbl []

(** Remove entries with quantity 0. *)
let filter_zero v =
  let assets = List.filter_map (fun (pid, names) ->
    let names = List.filter (fun (_, qty) -> qty <> 0L) names in
    if names = [] then None else Some (pid, names)
  ) v.assets in
  { v with assets }

(* ================================================================ *)
(* Arithmetic                                                        *)
(* ================================================================ *)

let add a b = {
  lovelace = Int64.add a.lovelace b.lovelace;
  assets = merge_assets a.assets b.assets;
}

let negate_assets assets =
  List.map (fun (pid, names) ->
    (pid, List.map (fun (name, qty) -> (name, Int64.neg qty)) names)
  ) assets

let subtract a b = {
  lovelace = Int64.sub a.lovelace b.lovelace;
  assets = merge_assets a.assets (negate_assets b.assets);
}

(* ================================================================ *)
(* Comparison                                                        *)
(* ================================================================ *)

(** Normalize assets for comparison: sort by policy_id, then by name. *)
let normalize_assets assets =
  let sorted = List.sort (fun (a, _) (b, _) -> Bytes.compare a b) assets in
  List.map (fun (pid, names) ->
    (pid, List.sort (fun (a, _) (b, _) -> Bytes.compare a b) names)
  ) sorted

let equal a b =
  if not (Int64.equal a.lovelace b.lovelace) then false
  else
    let fa = filter_zero a in
    let fb = filter_zero b in
    let na = normalize_assets fa.assets in
    let nb = normalize_assets fb.assets in
    List.length na = List.length nb &&
    List.for_all2 (fun (pa, nsa) (pb, nsb) ->
      Bytes.equal pa pb && List.length nsa = List.length nsb &&
      List.for_all2 (fun (na, qa) (nb, qb) ->
        Bytes.equal na nb && Int64.equal qa qb
      ) nsa nsb
    ) na nb

let is_positive v =
  v.lovelace >= 0L &&
  List.for_all (fun (_, names) ->
    List.for_all (fun (_, qty) -> qty >= 0L) names
  ) v.assets

(* ================================================================ *)
(* CBOR parsing                                                      *)
(* ================================================================ *)

(** Parse a CBOR value field: coin or [coin, multiasset]. *)
let of_cbor = function
  | Cbor.Uint n -> { lovelace = n; assets = [] }
  | Cbor.Array [Cbor.Uint n; Cbor.Map policy_map] ->
    let assets = List.filter_map (fun (k, v) ->
      match k, v with
      | Cbor.Bytes pid, Cbor.Map name_map when Bytes.length pid = 28 ->
        let names = List.filter_map (fun (nk, nv) ->
          match nk with
          | Cbor.Bytes name ->
            let qty = match nv with
              | Cbor.Uint q -> q
              | Cbor.Nint q -> q
              | _ -> 0L in
            Some (name, qty)
          | _ -> None
        ) name_map in
        Some (pid, names)
      | _ -> None
    ) policy_map in
    { lovelace = n; assets }
  | _ -> zero

(** Parse a mint field (key 9): multiasset<int64> with negative quantities. *)
let mint_of_cbor = function
  | Cbor.Map policy_map ->
    let assets = List.filter_map (fun (k, v) ->
      match k, v with
      | Cbor.Bytes pid, Cbor.Map name_map ->
        let names = List.filter_map (fun (nk, nv) ->
          match nk with
          | Cbor.Bytes name ->
            let qty = match nv with
              | Cbor.Uint q -> q
              | Cbor.Nint q -> q
              | _ -> 0L in
            Some (name, qty)
          | _ -> None
        ) name_map in
        Some (pid, names)
      | _ -> None
    ) policy_map in
    { lovelace = 0L; assets }
  | _ -> zero
