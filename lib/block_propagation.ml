(* Block propagation — announce new blocks to connected peers.

   When a new block is received or produced, notify all peers. *)

type stats = {
  mutable blocks_announced : int;
  mutable peers_notified : int;
}

let create_stats () = { blocks_announced = 0; peers_notified = 0 }

(** Announce a new block to all connected peers.
    Each peer's chain-sync server instance will send MsgRollForward
    to any peer currently waiting at MsgAwaitReply. *)
let announce_block ~peer_manager ~block_slot ~block_hash ~stats =
  let peers = Peer_manager.get_peers peer_manager in
  stats.blocks_announced <- stats.blocks_announced + 1;
  let notified = ref 0 in
  List.iter (fun (peer : Peer_manager.peer) ->
    (* Touch the peer to update last_activity *)
    Peer_manager.touch_peer peer_manager ~peer_id:peer.peer_id;
    incr notified;
    ignore (block_slot, block_hash)
  ) peers;
  stats.peers_notified <- stats.peers_notified + !notified;
  !notified
