(** Local node-to-client protocol server.

    Serves wallet queries over a Unix domain socket. *)

type context = {
  store : Store.store;
  ledger : Ledger_state.t;
  genesis : Genesis.genesis_config;
}

val handle_client : context -> Unix.file_descr -> unit
