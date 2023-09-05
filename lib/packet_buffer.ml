open Core

module BytePacketBuffer = struct
  type t =
    { mutable buf : Buffer.t
    ; mutable pos : int
    }

  let create () = { buf = Buffer.create 512; pos = 0 }
  let step t steps = t.pos <- t.pos + steps
  let seek t pos = t.pos <- pos

  let read t =
    if t.pos >= 512
    then raise_s [%message "Error: End of buffer" ~loc:[%here]]
    else (
      let res = Buffer.nth t.buf t.pos in
      t.pos <- t.pos + 1;
      res)
  ;;
end
