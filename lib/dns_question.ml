open Core 

type t =
    { name : string
    ; qtype : Query_type.t
    }
  [@@deriving show { with_path = false }]

  let read_qname (p_buffer : Packet_buffer.t) =
    let domain_name = ref "" in
    let local_pos = ref p_buffer.pos in
    let jumped = ref false in
    let max_jumps = 5 in
    let jumps_performed = ref 0 in
    let delim = ref "" in
    let rec loop () =
      match !jumps_performed > max_jumps with
      | true -> raise_s [%message "Error: Limit of jumps exceeded" ~loc:[%here]]
      | false ->
        let len = Packet_buffer.get p_buffer !local_pos in
        (* If len has the two most sign. bits set then we need to jump *)
        (match phys_equal (len land 0xC0) 0xC0 with
         | true ->
           if not !jumped then Packet_buffer.seek p_buffer (!local_pos + 2);
           let next_byte = Packet_buffer.get p_buffer (!local_pos + 1) in
           let offset = ((len lxor 0xC0) lsl 8) lor next_byte in
           local_pos := offset;
           jumped := true;
           jumps_performed := !jumps_performed + 1;
           loop ()
         | false ->
           local_pos := !local_pos + 1;
           if not @@ phys_equal len 0
           then (
             domain_name := !domain_name ^ !delim;
             let str_buffer = Packet_buffer.get_range p_buffer ~pos:!local_pos ~len in
             domain_name
               := !domain_name ^ (Cstruct.to_string str_buffer |> String.lowercase);
             delim := ".";
             local_pos := !local_pos + len;
             loop ()))
    in
    loop ();
    if not !jumped then Packet_buffer.seek p_buffer !local_pos;
    !domain_name
  ;;

  let write_qname (p_buffer : Packet_buffer.t) qname =
    List.iter (String.split qname ~on:'.') ~f:(fun label ->
      let len = String.length label in
      if len > 0x3f
      then raise_s [%message "Error: Single label exceeds 63 chars length" ~loc:[%here]]
      else Packet_buffer.write p_buffer len;
      String.iter label ~f:(fun b -> Packet_buffer.write p_buffer (Char.to_int b)));
    Packet_buffer.write p_buffer 0
  ;;

  let read buffer =
    let name = read_qname buffer in
    let qtype = Packet_buffer.read_u16 buffer |> Query_type.t_of_num in
    let _ = Packet_buffer.read_u16 buffer in
    { name; qtype }
  ;;

  let write buffer t =
    write_qname buffer t.name;
    let typenum = Query_type.num_of_t t.qtype in
    Packet_buffer.write_u16 buffer typenum;
    Packet_buffer.write_u16 buffer 1
  ;;
