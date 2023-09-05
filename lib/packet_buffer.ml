open Core

module PacketBuffer = struct
  type t =
    { mutable buf : Bigbuffer.t
    ; mutable pos : int
    }

  let create () = { buf = Bigbuffer.create 512; pos = 0 }
  let step t steps = t.pos <- t.pos + steps
  let seek t pos = t.pos <- pos

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
    else Bigbuffer.nth t.buf t.pos |> Char.to_int
  ;;

  let get_range t ~pos ~len =
    if pos + len >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Bigbuffer.sub t.buf ~pos ~len
  ;;

  let read_u16 t =
    let first_byte = read t lsl 8 in
    let second_byte = read t in
    first_byte lor second_byte
  ;;

  let read_u32 t =
    let first_byte = read t lsl 24 in
    let second_byte = read t lsl 16 in
    let remaining_u16 = read_u16 t in
    first_byte lor second_byte lor remaining_u16
  ;;
end
