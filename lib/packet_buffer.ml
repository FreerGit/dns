open Core

module PacketBuffer = struct
  type t =
    { mutable buf : Cstruct.t
    ; mutable pos : int
    }

  let create bytes =
    let self = { buf = Cstruct.create 512; pos = 0 } in
    Cstruct.blit_from_string bytes 0 self.buf 0 (String.length bytes);
    self
  ;;

  let step t steps = t.pos <- t.pos + steps
  let seek t pos = t.pos <- pos

  let read t =
    if t.pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else (
      let res = Cstruct.get t.buf t.pos in
      t.pos <- t.pos + 1;
      Char.to_int res)
  ;;

  let get t pos =
    if pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Cstruct.get t.buf pos |> Char.to_int
  ;;

  let get_range t ~pos ~len =
    if pos + len >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Cstruct.sub t.buf pos len
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

  let write t u8 =
    if t.pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else Cstruct.set_uint8 t.buf t.pos u8;
    t.pos <- t.pos + 1
  ;;

  let write_u16 t u16 =
    write t (u16 lsr 8);
    write t (u16 land 0xFF)
  ;;

  let write_u32 t u32 =
    write t ((u32 lsr 24) land 0xFF);
    write t ((u32 lsr 16) land 0xFF);
    write t ((u32 lsr 8) land 0xFF);
    write t ((u32 lsr 0) land 0xFF)
  ;;
end
