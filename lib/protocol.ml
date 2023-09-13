open Core
open Packet_buffer


module DnsQuestion = struct
  type t =
    { name : string
    ; qtype : Query_type.t
    }
  [@@deriving show { with_path = false }]

  let read_qname (p_buffer) =
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
end

module DnsRecord = struct
  type t =
    | UNKOWN of
        { domain : string
        ; qtype : int
        ; data_len : int
        ; ttl : int
        }
    | A of
        { domain : string
        ; addr : Ipaddr.V4.t
        ; ttl : int
        }
    | NS of
        { domain : string
        ; host : string
        ; ttl : int
        }
    | CNAME of
        { domain : string
        ; host : string
        ; ttl : int
        }
    | MX of
        { domain : string
        ; priority : int
        ; host : string
        ; ttl : int
        }
    | AAAA of
        { domain : string
        ; addr : Ipaddr.V6.t
        ; ttl : int
        }
  [@@deriving show { with_path = false }]

  let read buffer =
    let domain = DnsQuestion.read_qname buffer in
    let qtype_num = Packet_buffer.read_u16 buffer in
    let qtype = Query_type.t_of_num qtype_num in
    let _ = Packet_buffer.read_u16 buffer in
    let ttl = Packet_buffer.read_u32 buffer in
    let data_len = Packet_buffer.read_u16 buffer in
    match qtype with
    | A ->
      let raw_addr = Packet_buffer.read_u32 buffer in
      let addr =
        Ipaddr.V4.make
          (raw_addr lsr 24 |> ( land ) 0xFF)
          (raw_addr lsr 16 |> ( land ) 0xFF)
          (raw_addr lsr 8 |> ( land ) 0xFF)
          (raw_addr lsr 0 |> ( land ) 0xFF)
      in
      A { domain; addr; ttl }
    | AAAA ->
      let raw_addr1 = Packet_buffer.read_u32 buffer in
      let raw_addr2 = Packet_buffer.read_u32 buffer in
      let raw_addr3 = Packet_buffer.read_u32 buffer in
      let raw_addr4 = Packet_buffer.read_u32 buffer in
      let addr =
        Ipaddr.V6.make
          ((raw_addr1 lsr 16) land 0xFFFF)
          ((raw_addr1 lsr 0) land 0xFFFF)
          ((raw_addr2 lsr 16) land 0xFFFF)
          ((raw_addr2 lsr 0) land 0xFFFF)
          ((raw_addr3 lsr 16) land 0xFFFF)
          ((raw_addr3 lsr 0) land 0xFFFF)
          ((raw_addr4 lsr 16) land 0xFFFF)
          ((raw_addr4 lsr 0) land 0xFFFF)
      in
      AAAA { domain; addr; ttl }
    | NS ->
      let name = DnsQuestion.read_qname buffer in
      NS { domain; host = name; ttl }
    | CNAME ->
      let name = DnsQuestion.read_qname buffer in
      CNAME { domain; host = name; ttl }
    | MX ->
      let priority = Packet_buffer.read_u16 buffer in
      let name = DnsQuestion.read_qname buffer in
      MX { domain; priority; host = name; ttl }
    | UNKOWN _ ->
      Packet_buffer.step buffer data_len;
      UNKOWN { domain; qtype = qtype_num; data_len; ttl }
  ;;

  let write (buffer : Packet_buffer.t) t =
    let start_pos = buffer.pos in
    let open Packet_buffer in
    let _ =
      match t with
      | A a ->
        DnsQuestion.write_qname buffer a.domain;
        write_u16 buffer (Query_type.num_of_t Query_type.A);
        write_u16 buffer 1;
        write_u32 buffer a.ttl;
        write_u16 buffer 4;
        let octets = Ipaddr.V4.to_octets a.addr in
        write buffer (String.get octets 0 |> Char.to_int);
        write buffer (String.get octets 1 |> Char.to_int);
        write buffer (String.get octets 2 |> Char.to_int);
        write buffer (String.get octets 3 |> Char.to_int)
      | NS ns ->
        DnsQuestion.write_qname buffer ns.domain;
        write_u16 buffer (Query_type.NS |> Query_type.num_of_t);
        write_u16 buffer 1;
        write_u16 buffer ns.ttl;
        let pos = buffer.pos in
        write_u16 buffer 0;
        DnsQuestion.write_qname buffer ns.host;
        let size = buffer.pos - (pos + 2) in
        set_u16 buffer ~pos ~u16:size
      | CNAME cn ->
        DnsQuestion.write_qname buffer cn.domain;
        write_u16 buffer (Query_type.NS |> Query_type.num_of_t);
        write_u16 buffer 1;
        write_u16 buffer cn.ttl;
        let pos = buffer.pos in
        write_u16 buffer 0;
        DnsQuestion.write_qname buffer cn.host;
        let size = buffer.pos - (pos + 2) in
        set_u16 buffer ~pos ~u16:size
      | MX mx ->
        DnsQuestion.write_qname buffer mx.domain;
        write_u16 buffer (Query_type.NS |> Query_type.num_of_t);
        write_u16 buffer 1;
        write_u16 buffer mx.ttl;
        let pos = buffer.pos in
        write_u16 buffer 0;
        write_u16 buffer mx.priority;
        DnsQuestion.write_qname buffer mx.host;
        let size = buffer.pos - (pos + 2) in
        set_u16 buffer ~pos ~u16:size
      | AAAA quad_a ->
        DnsQuestion.write_qname buffer quad_a.domain;
        write_u16 buffer (Query_type.AAAA |> Query_type.num_of_t);
        write_u16 buffer 1;
        write_u32 buffer quad_a.ttl;
        write_u16 buffer 16;
        Ipaddr.V6.to_octets quad_a.addr |> int_of_string |> write_u16 buffer
      | UNKOWN u -> Format.printf "Skipping record %s" u.domain
    in
    buffer.pos - start_pos
  ;;
end

module DnsPacket = struct
  type t =
    { mutable header : Dns_header.t
    ; mutable questions : DnsQuestion.t list
    ; mutable answers : DnsRecord.t list
    ; mutable authorities : DnsRecord.t list
    ; mutable resources : DnsRecord.t list
    }
  [@@deriving show { with_path = false }]

  let make header =
    { header; questions = []; answers = []; authorities = []; resources = [] }
  ;;

  let read buffer =
    let header = Dns_header.read buffer in
    let packet =
      { header; questions = []; answers = []; authorities = []; resources = [] }
    in
    for _ = 1 to header.questions do
      let question = DnsQuestion.read buffer in
      packet.questions <- List.cons question packet.questions
    done;
    for _ = 1 to header.answers do
      let answer = DnsRecord.read buffer in
      packet.answers <- List.cons answer packet.answers
    done;
    for _ = 1 to header.authoritative_entries do
      let authority = DnsRecord.read buffer in
      packet.authorities <- List.cons authority packet.authorities
    done;
    for _ = 1 to header.resource_entries do
      let resource = DnsRecord.read buffer in
      packet.resources <- List.cons resource packet.resources
    done;
    packet
  ;;

  let write buffer t =
    t.header <- { t.header with questions = List.length t.questions };
    t.header <- { t.header with answers = List.length t.answers };
    t.header <- { t.header with authoritative_entries = List.length t.authorities };
    t.header <- { t.header with resource_entries = List.length t.resources };
    Dns_header.write buffer t.header;
    List.iter t.questions ~f:(DnsQuestion.write buffer);
    List.iter t.answers ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    List.iter t.authorities ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    List.iter t.resources ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    ()
  ;;
end
