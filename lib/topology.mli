(** Topology configuration — peer addresses and connection policy. *)

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

val parse_topology : path:string -> (topology, string) result
val default_topology : string -> topology
val all_entries : topology -> topology_entry list
