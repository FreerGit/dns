open Core

module PacketBuffer = struct
  type t =
    { mutable buf : Bigbuffer.t
    ; mutable pos : int
    }
  [@@deriving sexp_of]

  let create bytes =
    let self = { buf = Bigbuffer.create 512; pos = 0 } in
    Bigbuffer.add_bytes self.buf bytes;
    self
  ;;

  let step t steps = t.pos <- t.pos + steps
  let seek t pos = t.pos <- pos
  let reset_pos t = t.pos <- 0

  let read t =
    if t.pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else (
      let res = Bigbuffer.nth t.buf t.pos in
      t.pos <- t.pos + 1;
      Char.to_int res)
  ;;

  let get t pos =
    if pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Bigbuffer.nth t.buf pos |> Char.to_int
  ;;

  let get_range t ~pos ~len =
    if pos + len >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Bigbuffer.sub t.buf ~pos ~len
  ;;

  let read_u16 t =
    let a = read t lsl 8 in
    let b = read t in
    let x = a lor b in
    x
  ;;

  let read_u32 t =
    let a = read t lsl 24 in
    let b = read t lsl 16 in
    let c = read t lsl 8 in
    let d = read t lsl 0 in
    a lor b lor c lor d
  ;;
  (* a lor b |> ( lor ) (read t lsl 8) |> ( lor ) (read t lsl 0) *)
end
