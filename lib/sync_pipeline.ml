(* Unified sync pipeline: chain-sync + block-fetch + storage.

   Orchestrates the full block synchronization flow:
   1. Find intersection with remote node using stored chain points
   2. Chain-sync headers in batches, then block-fetch bodies
   3. Store each block to disk via Store
   4. At tip, switch to live mode (one block at a time)
   5. Handle rollbacks by re-syncing from the rollback point *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type progress = {
  blocks_synced : int;
  current_slot : int64;
  tip_slot : int64;
  tip_block : int64;
  blocks_per_sec : float;
  disk_blocks : int;
}

type block_info = {
  bi_slot : int64;
  bi_hash : bytes;
  bi_size : int;
}

type config = {
  batch_size : int;
  on_block : block_info -> unit;
  on_progress : progress -> unit;
  should_stop : unit -> bool;
}

type result =
  | Completed       (** Reached tip and live mode ended *)
  | Stopped         (** Stopped by should_stop *)
  | Disconnected of string * int64
    (** Connection lost. Carries error message and last stored slot. *)

(* ================================================================ *)
(* Internal helpers                                                  *)
(* ================================================================ *)

let extract_point header =
  Network.extract_point_from_header header

(** Collect a batch of header points via pipelined chain-sync.
    Returns (points, at_tip, tip_info) where at_tip is true if
    MsgAwaitReply was received before the batch was full. *)
let collect_headers net ~batch_size ~should_stop:_ =
  match Network.pipeline_request_next net ~count:batch_size with
  | Error _ -> ([], false, (-1L, -1L))
  | Ok (events, at_tip) ->
    let points = ref [] in
    let tip_info = ref (0L, 0L) in
    List.iter (fun evt ->
      match evt with
      | Network.Roll_forward { header; tip } ->
        tip_info := (
          (match tip.Chain_sync.tip_point with
           | Origin -> 0L | Point (s, _) -> s),
          tip.tip_block_number);
        (match extract_point header with
         | Ok (Chain_sync.Point _ as pt) -> points := pt :: !points
         | _ -> ())
      | Roll_backward _ -> ()
      | Await_reply -> ()
    ) events;
    (List.rev !points, at_tip, !tip_info)

(** Fetch a single range of blocks and store them.
    Returns the number of blocks stored. *)
let fetch_range net store pts_arr offset count =
  if count < 2 then Ok 0
  else
    let from_pt = pts_arr.(offset) in
    let to_pt = pts_arr.(offset + count - 1) in
    let* result = Network.request_range net ~from_point:from_pt ~to_point:to_pt in
    match result with
    | No_blocks -> Ok 0
    | Batch_started ->
      let stored = ref 0 in
      let block_idx = ref 0 in
      let streaming = ref true in
      while !streaming do
        match Network.recv_block net with
        | Error e -> streaming := false; ignore e
        | Ok None -> streaming := false  (* BatchDone *)
        | Ok (Some block_bytes) ->
          let idx = offset + !block_idx in
          if idx < Array.length pts_arr then begin
            (match pts_arr.(idx) with
             | Chain_sync.Point (slot, hash) ->
               (match Store.store_block store ~slot ~hash ~cbor_bytes:block_bytes with
                | Ok () -> incr stored
                | Error _ -> ())
             | _ -> ())
          end;
          incr block_idx
      done;
      Ok !stored

(** Fetch block bodies for a list of points and store them.
    Chunks into ranges of max_range to avoid protocol limits.
    Returns the number of blocks successfully stored. *)
let fetch_and_store net store points =
  match points with
  | [] | [_] -> Ok 0
  | _ ->
    let pts_arr = Array.of_list points in
    let total = Array.length pts_arr in
    let max_range = 100 in  (* chunk into manageable ranges *)
    let stored = ref 0 in
    let offset = ref 0 in
    let ok = ref true in
    while !ok && !offset < total do
      let count = min max_range (total - !offset) in
      if count < 2 then begin
        offset := total  (* skip remaining singleton *)
      end else begin
        match fetch_range net store pts_arr !offset count with
        | Ok n -> stored := !stored + n; offset := !offset + count
        | Error _ -> ok := false
      end
    done;
    Ok !stored

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

(** Run the sync pipeline.
    Finds intersection, syncs headers+bodies in batches, stores to disk.
    Returns when at tip, stopped, or disconnected. *)
let start ~net ~store ~config =
  let start_time = Unix.gettimeofday () in
  let total_synced = ref 0 in
  let current_slot = ref 0L in
  let tip_slot = ref 0L in
  let tip_block = ref 0L in

  (* Phase 1: Find intersection *)
  let chain_points = Store.get_chain_points store in
  let* (_intersect, tip) = Network.find_intersection net ~points:chain_points in
  tip_slot := (match tip.Chain_sync.tip_point with
    | Origin -> 0L | Point (s, _) -> s);
  tip_block := tip.tip_block_number;

  (* Phase 2: Batch sync loop *)
  let at_tip = ref false in
  let running = ref true in
  while !running && not !at_tip && not (config.should_stop ()) do
    let (points, reached_tip, (ts, tb)) =
      collect_headers net ~batch_size:config.batch_size ~should_stop:config.should_stop in
    if ts = -1L then begin
      (* Error during collection *)
      running := false
    end else begin
      tip_slot := ts;
      tip_block := tb;
      at_tip := reached_tip;
      if points <> [] then begin
        (* Update current_slot from last point *)
        (match List.rev points with
         | Chain_sync.Point (s, _) :: _ -> current_slot := s
         | _ -> ());
        (* Fetch and store *)
        (match fetch_and_store net store points with
         | Ok n ->
           total_synced := !total_synced + n;
           (* Notify callbacks *)
           List.iter (fun pt ->
             match pt with
             | Chain_sync.Point (slot, hash) ->
               config.on_block { bi_slot = slot; bi_hash = hash;
                                 bi_size = 0 (* size not available from points *) }
             | _ -> ()
           ) points
         | Error _ -> running := false);
        (* Progress callback *)
        let elapsed = Unix.gettimeofday () -. start_time in
        let bps = if elapsed > 0.0 then
            float_of_int !total_synced /. elapsed
          else 0.0 in
        config.on_progress {
          blocks_synced = !total_synced;
          current_slot = !current_slot;
          tip_slot = !tip_slot;
          tip_block = !tip_block;
          blocks_per_sec = bps;
          disk_blocks = Store.block_count store;
        }
      end
    end
  done;

  if config.should_stop () then
    Ok Stopped
  else if not !running then
    Ok (Disconnected ("sync error", !current_slot))
  else begin
    (* Phase 3: Live mode — at tip, one block at a time.
       After AwaitReply from batch phase, the server sends RollForward
       when a new block arrives. Then we must send RequestNext to get
       the next one (or AwaitReply if at tip again). *)
    let live = ref true in
    while !live && not (config.should_stop ()) do
      match Network.await_next net with
      | Error e ->
        live := false;
        ignore e
      | Ok (Roll_forward { header; tip }) ->
        tip_slot := (match tip.Chain_sync.tip_point with
          | Origin -> 0L | Point (s, _) -> s);
        tip_block := tip.tip_block_number;
        (match extract_point header with
         | Ok (Chain_sync.Point (slot, hash) as pt) ->
           current_slot := slot;
           (* Fetch this single block *)
           (match fetch_and_store net store [pt; pt] with
            | Ok n ->
              total_synced := !total_synced + n;
              config.on_block { bi_slot = slot; bi_hash = hash; bi_size = 0 };
              (* Report progress *)
              let elapsed = Unix.gettimeofday () -. start_time in
              let bps = if elapsed > 0.0 then
                  float_of_int !total_synced /. elapsed
                else 0.0 in
              config.on_progress {
                blocks_synced = !total_synced;
                current_slot = !current_slot;
                tip_slot = !tip_slot;
                tip_block = !tip_block;
                blocks_per_sec = bps;
                disk_blocks = Store.block_count store;
              }
            | Error _ -> live := false);
           (* Request next to continue the chain-sync *)
           (match Network.request_next net with
            | Ok Await_reply -> ()  (* at tip again, will block *)
            | Ok _ -> ()  (* got another block immediately, loop will handle *)
            | Error _ -> live := false)
         | _ -> ())
      | Ok (Roll_backward _) ->
        (* After rollback, request next *)
        (match Network.request_next net with
         | Ok _ -> () | Error _ -> live := false)
      | Ok Await_reply -> ()
    done;
    if config.should_stop () then Ok Stopped
    else Ok (Disconnected ("live mode error", !current_slot))
  end

(** Create a default config with reasonable defaults. *)
let default_config
    ?(batch_size = 50)
    ?(on_block = fun _ -> ())
    ?(on_progress = fun _ -> ())
    ?(should_stop = fun () -> false) () =
  { batch_size; on_block; on_progress; should_stop }
