open Core
open Packet_buffer

module ResultCode = struct
  type t =
    | NOERROR
    | FORMERR
    | SERVFAIL
    | NXDOMAIN
    | NOTIMP
    | REFUSED
  [@@deriving show { with_path = false }]

  let of_t num =
    match num with
    | 1 -> FORMERR
    | 2 -> SERVFAIL
    | 3 -> NXDOMAIN
    | 4 -> NOTIMP
    | 5 -> REFUSED
    | 0 | _ -> NOERROR
  ;;
end

module DnsHeader = struct
  [%%cstruct
  type t =
    { id : uint16_t
    ; flags : uint16_t
    ; questions : uint16_t
    ; answers : uint16_t
    ; authoritative_entries : uint16_t
    ; resource_entries : uint16_t
    }
  [@@big_endian]]

  type t =
    { id : int
    ; recursion_desired : bool
    ; truncated_message : bool
    ; authoritative_answer : bool
    ; opcode : int
    ; query_response : bool
    ; rescode : ResultCode.t
    ; checking_disabled : bool
    ; authed_data : bool
    ; z : bool
    ; recursion_available : bool
    ; questions : int
    ; answers : int
    ; authoritative_entries : int
    ; resource_entries : int
    }
  [@@deriving show { with_path = false }]

  let read buffer =
    let id = PacketBuffer.read_u16 buffer in
    let flags = PacketBuffer.read_u16 buffer in
    let a = flags lsr 8 in
    let b = flags land 0xFF in
    let questions = PacketBuffer.read_u16 buffer in
    let answers = PacketBuffer.read_u16 buffer in
    let authoritative_entries = PacketBuffer.read_u16 buffer in
    let resource_entries = PacketBuffer.read_u16 buffer in
    let result : t =
      { id
      ; recursion_desired = a land (1 lsl 0) > 0
      ; truncated_message = a land (1 lsl 1) > 0
      ; authoritative_answer = a land (1 lsl 2) > 0
      ; opcode = (a lsr 3) land 0x0F
      ; query_response = a land (1 lsl 7) > 0
      ; rescode = ResultCode.of_t (b land 0x0F)
      ; checking_disabled = b land (1 lsl 4) > 0
      ; authed_data = b land (1 lsl 5) > 0
      ; z = b land (1 lsl 6) > 0
      ; recursion_available = b land (1 lsl 7) > 0
      ; questions
      ; answers
      ; authoritative_entries
      ; resource_entries
      }
    in
    result
  ;;
end

module QueryType = struct
  type t =
    | A
    | UNKOWN of int
  [@@deriving show { with_path = false }]

  let num_of_t t =
    match t with
    | A -> 1
    | UNKOWN x -> x
  ;;

  let t_of_num num =
    match num with
    | 1 -> A
    | _ -> UNKOWN num
  ;;
end

module DnsQuestion = struct
  type t =
    { name : string
    ; qtype : QueryType.t
    }
  [@@deriving show { with_path = false }]

  let read_qname (p_buffer : PacketBuffer.t) =
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
        let len = PacketBuffer.get p_buffer !local_pos in
        (* If len has the two most sign. bits set then we need to jump *)
        (match phys_equal (len land 0xC0) 0xC0 with
         | true ->
           if not !jumped then PacketBuffer.seek p_buffer (!local_pos + 2);
           let next_byte = PacketBuffer.get p_buffer (!local_pos + 1) in
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
             let str_buffer = PacketBuffer.get_range p_buffer ~pos:!local_pos ~len in
             domain_name := !domain_name ^ (Bytes.to_string str_buffer |> String.lowercase);
             delim := ".";
             local_pos := !local_pos + len;
             loop ()))
    in
    loop ();
    if not !jumped then PacketBuffer.seek p_buffer !local_pos;
    !domain_name
  ;;

  let read buffer =
    let name = read_qname buffer in
    let qtype = PacketBuffer.read_u16 buffer |> QueryType.t_of_num in
    let _ = PacketBuffer.read_u16 buffer in
    { name; qtype }
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
  [@@deriving show { with_path = false }]

  let read buffer =
    let domain = DnsQuestion.read_qname buffer in
    let qtype_num = PacketBuffer.read_u16 buffer in
    let qtype = QueryType.t_of_num qtype_num in
    (* Ignore QCLASS for now *)
    let _ = PacketBuffer.read_u16 buffer in
    let ttl = PacketBuffer.read_u32 buffer in
    let data_len = PacketBuffer.read_u16 buffer in
    match qtype with
    | A ->
      let raw_addr = PacketBuffer.read_u32 buffer in
      let addr =
        Ipaddr.V4.make
          (raw_addr lsr 24 |> ( land ) 0xFF)
          (raw_addr lsr 16 |> ( land ) 0xFF)
          (raw_addr lsr 8 |> ( land ) 0xFF)
          (raw_addr lsr 0 |> ( land ) 0xFF)
      in
      A { domain; addr; ttl }
    | UNKOWN _ ->
      PacketBuffer.step buffer data_len;
      UNKOWN { domain; qtype = qtype_num; data_len; ttl }
  ;;
end

module DnsPacket = struct
  type t =
    { header : DnsHeader.t
    ; questions : DnsQuestion.t list
    ; answers : DnsRecord.t list
    }
  [@@deriving show { with_path = false }]

  let read buffer =
    (* let buffer = PacketBuffer.create (Cstruct.to_bytes bytes) in *)
    let header = DnsHeader.read buffer in
    (* let without_header = Cstruct.shift bytes 12 in *)
    (* Cstruct.hexdump without_header; *)
    let questions = ref [] in
    let answers = ref [] in
    (* TODO: iterate based on number off questions in header *)
    for _ = 1 to header.questions do
      let question = DnsQuestion.read buffer in
      questions := List.cons question !questions
      (* PacketBuffer.reset_pos buffer *)
    done;
    for _ = 1 to header.answers do
      let answer = DnsRecord.read buffer in
      answers := List.cons answer !answers
      (* PacketBuffer.reset_pos buffer *)
    done;
    { header; questions = !questions; answers = !answers }
  ;;
end
