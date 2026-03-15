(* Epoch and slot arithmetic for Cardano.

   Byron uses 21600 slots per epoch (20s slots, 5 days).
   Shelley+ uses 432000 slots per epoch (1s slots, 5 days).
   Preview testnet starts directly in Shelley with epoch 0 at slot 0. *)

type epoch_params = {
  byron_epoch_length : int64;       (** 21600 for mainnet, 4320 for preview *)
  shelley_epoch_length : int64;     (** 432000 *)
  shelley_start_slot : int64;       (** First Shelley slot *)
  shelley_start_epoch : int64;      (** Epoch number at shelley_start_slot *)
}

let mainnet_epoch_params = {
  byron_epoch_length = 21600L;
  shelley_epoch_length = 432000L;
  shelley_start_slot = 4492800L;    (* Mainnet Shelley start *)
  shelley_start_epoch = 208L;
}

let preview_epoch_params = {
  byron_epoch_length = 4320L;
  shelley_epoch_length = 432000L;
  shelley_start_slot = 0L;
  shelley_start_epoch = 0L;
}

let slot_to_epoch params slot =
  if Int64.compare slot params.shelley_start_slot < 0 then
    (* Byron era *)
    Int64.div slot params.byron_epoch_length
  else
    let shelley_slots = Int64.sub slot params.shelley_start_slot in
    let shelley_epochs = Int64.div shelley_slots params.shelley_epoch_length in
    Int64.add params.shelley_start_epoch shelley_epochs

let epoch_to_first_slot params epoch =
  if Int64.compare epoch params.shelley_start_epoch < 0 then
    Int64.mul epoch params.byron_epoch_length
  else
    let epochs_since = Int64.sub epoch params.shelley_start_epoch in
    Int64.add params.shelley_start_slot
      (Int64.mul epochs_since params.shelley_epoch_length)

let slot_in_epoch params slot =
  if Int64.compare slot params.shelley_start_slot < 0 then
    Int64.rem slot params.byron_epoch_length
  else
    let shelley_slots = Int64.sub slot params.shelley_start_slot in
    Int64.rem shelley_slots params.shelley_epoch_length

let is_epoch_boundary params ~prev_slot ~slot =
  let e1 = slot_to_epoch params prev_slot in
  let e2 = slot_to_epoch params slot in
  not (Int64.equal e1 e2)
