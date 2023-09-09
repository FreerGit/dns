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

  let to_int code =
    match code with
    | NOERROR -> 0
    | FORMERR -> 1
    | SERVFAIL -> 2
    | NXDOMAIN -> 3
    | NOTIMP -> 4
    | REFUSED -> 5
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

  let make () =
    let header : t =
      { id = 0
      ; recursion_desired = false
      ; truncated_message = false
      ; authoritative_answer = false
      ; opcode = 0
      ; query_response = false
      ; rescode = ResultCode.NOERROR
      ; checking_disabled = false
      ; authed_data = false
      ; z = false
      ; recursion_available = false
      ; questions = 0
      ; answers = 0
      ; authoritative_entries = 0
      ; resource_entries = 0
      }
    in
    header
  ;;

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

  let write buffer t =
    PacketBuffer.write_u16 buffer t.id;
    let to_int = Bool.to_int in
    PacketBuffer.write
      buffer
      (to_int t.recursion_desired
       lor (to_int t.truncated_message lsl 1)
       lor (to_int t.authoritative_answer lsl 2)
       lor (t.opcode lsl 3)
       lor (to_int t.query_response lsl 7));
    PacketBuffer.write
      buffer
      (ResultCode.to_int t.rescode
       lor (to_int t.checking_disabled lsl 4)
       lor (to_int t.authed_data lsl 5)
       lor (to_int t.z lsl 6)
       lor (to_int t.recursion_available lsl 7));
    PacketBuffer.write_u16 buffer t.questions;
    PacketBuffer.write_u16 buffer t.answers;
    PacketBuffer.write_u16 buffer t.authoritative_entries;
    PacketBuffer.write_u16 buffer t.resource_entries;
    Cstruct.hexdump buffer.buf
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
             domain_name
               := !domain_name ^ (Cstruct.to_string str_buffer |> String.lowercase);
             delim := ".";
             local_pos := !local_pos + len;
             loop ()))
    in
    loop ();
    if not !jumped then PacketBuffer.seek p_buffer !local_pos;
    !domain_name
  ;;

  let write_qname (p_buffer : PacketBuffer.t) qname =
    List.iter (String.split qname ~on:'.') ~f:(fun label ->
      let len = String.length label in
      if len > 0x3f
      then raise_s [%message "Error: Single label exceeds 63 chars length" ~loc:[%here]]
      else PacketBuffer.write p_buffer len;
      String.iter label ~f:(fun b -> PacketBuffer.write p_buffer (Char.to_int b));
      PacketBuffer.write p_buffer 0)
  ;;

  let read buffer =
    let name = read_qname buffer in
    let qtype = PacketBuffer.read_u16 buffer |> QueryType.t_of_num in
    let _ = PacketBuffer.read_u16 buffer in
    { name; qtype }
  ;;

  let write buffer t =
    write_qname buffer t.name;
    let typenum = QueryType.num_of_t t.qtype in
    PacketBuffer.write_u16 buffer typenum;
    PacketBuffer.write_u16 buffer 1
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

  let write (buffer : PacketBuffer.t) t =
    let start_pos = buffer.pos in
    let _ =
      match t with
      | UNKOWN u -> Format.printf "Skipping record %s" u.domain
      | A a ->
        DnsQuestion.write_qname buffer a.domain;
        PacketBuffer.write_u16 buffer (QueryType.num_of_t QueryType.A);
        PacketBuffer.write_u16 buffer 1;
        PacketBuffer.write_u16 buffer a.ttl;
        PacketBuffer.write_u16 buffer 4;
        let octets = Ipaddr.V4.to_octets a.addr in
        PacketBuffer.write buffer (String.get octets 0 |> Char.to_int);
        PacketBuffer.write buffer (String.get octets 1 |> Char.to_int);
        PacketBuffer.write buffer (String.get octets 2 |> Char.to_int);
        PacketBuffer.write buffer (String.get octets 3 |> Char.to_int)
    in
    buffer.pos - start_pos
  ;;
end

module DnsPacket = struct
  type t =
    { header : DnsHeader.t
    ; questions : DnsQuestion.t list
    ; answers : DnsRecord.t list
    ; authorities : DnsRecord.t list
    ; resources : DnsRecord.t list
    }
  [@@deriving show { with_path = false }]

  let make header =
    { header; questions = []; answers = []; authorities = []; resources = [] }
  ;;

  let read buffer =
    let header = DnsHeader.read buffer in
    let questions = ref [] in
    let answers = ref [] in
    let authorities = ref [] in
    let resources = ref [] in
    (* TODO: iterate based on number off questions in header *)
    for _ = 1 to header.questions do
      let question = DnsQuestion.read buffer in
      questions := List.cons question !questions
    done;
    for _ = 1 to header.answers do
      let answer = DnsRecord.read buffer in
      answers := List.cons answer !answers
    done;
    for _ = 1 to header.authoritative_entries do
      let authority = DnsRecord.read buffer in
      authorities := List.cons authority !authorities
    done;
    for _ = 1 to header.resource_entries do
      let resource = DnsRecord.read buffer in
      resources := List.cons resource !resources
    done;
    { header
    ; questions = !questions
    ; answers = !answers
    ; authorities = !authorities
    ; resources = !resources
    }
  ;;

  let write buffer t =
    DnsHeader.write buffer t.header;
    List.iter t.questions ~f:(DnsQuestion.write buffer);
    List.iter t.answers ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    List.iter t.authorities ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    List.iter t.resources ~f:(fun req -> DnsRecord.write buffer req |> ignore);
    ()
  ;;
end
