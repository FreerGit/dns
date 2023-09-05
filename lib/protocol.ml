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
  [@@deriving show]

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
    ; query_response : bool
    ; opcode : int
    ; authoritative_answer : bool
    ; truncated_message : bool
    ; recursion_desired : bool
    ; recursion_available : bool
    ; z : int
    ; rescode : ResultCode.t
    ; questions : int
    ; answers : int
    ; authoritative_entries : int
    ; resource_entries : int
    }
  [@@deriving show]

  let read bytes =
    let ( & ) a b = a land b in
    let get_id = get_t_id bytes in
    let flags = get_t_flags bytes in
    let qr = (flags lsr 15) & 1 in
    let opcode = (flags lsr 11) & 0xF in
    let aa = (flags lsr 10) & 1 in
    let tc = (flags lsr 9) & 1 in
    let rd = (flags lsr 8) & 1 in
    let ra = (flags lsr 7) & 1 in
    let z = (flags lsr 4) & 0x7 in
    let rcode = flags & 0xF in
    let result : t =
      { id = get_id
      ; query_response = Utils.bool_of_int qr
      ; opcode
      ; authoritative_answer = Utils.bool_of_int aa
      ; truncated_message = Utils.bool_of_int tc
      ; recursion_desired = Utils.bool_of_int rd
      ; recursion_available = Utils.bool_of_int ra
      ; z
      ; rescode = ResultCode.of_t rcode
      ; questions = get_t_questions bytes
      ; answers = get_t_answers bytes
      ; authoritative_entries = get_t_authoritative_entries bytes
      ; resource_entries = get_t_resource_entries bytes
      }
    in
    result
  ;;
end

module QueryType = struct
  (* [%%cenum
  type t = 
  (* | UNKNOWN of uint16_t *)
  | A
   [@@uint16_t]] *)

  type t =
    | A
    | UNKOWN of int
  [@@deriving show]

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
  [@@deriving show]

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
           let offset = len lxor ((0xC0 lsl 8) lor next_byte) in
           local_pos := offset;
           jumped := true;
           jumps_performed := !jumps_performed + 1;
           loop ()
         | false ->
           local_pos := !local_pos + 1;
           if not @@ phys_equal len 0 then domain_name := !delim;
           let str_buffer = PacketBuffer.get_range p_buffer ~pos:!local_pos ~len in
           domain_name := !domain_name ^ (Bytes.to_string str_buffer |> String.lowercase);
           delim := ".";
           local_pos := !local_pos + len)
    in
    loop ();
    if not !jumped then PacketBuffer.seek p_buffer !local_pos;
    domain_name
  ;;

  let read _bytes = []
end

module DnsRecord = struct
  (*
     [%%cenum
    type t =
    |  {
      domain: uint16_t
    ; qtype: uint16_t
      ; data_len: uint16_t
      ; ttl: uint32_t
      }
    | A of {
      domain: uint16_t
      ; addr: uint16_t
      ; ttl: uint32_t
    }
  [@@big_endian]] *)

  type t =
    | UNKOWN of
        { domain : string
        ; qtype : int
        ; data_len : int
        ; ttl : int
        }
    | A of
        { domain : string
        ; addr : string
        ; ttl : int
        }
end

module DnsPacket = struct
  type t =
    { header : DnsHeader.t
    ; questions : DnsQuestion.t list
    }
  [@@deriving show]

  let read bytes =
    let header = DnsHeader.read bytes in
    let without_header = Cstruct.shift bytes 12 in
    (* TODO: iterate based on number off questions in header *)
    let questions = DnsQuestion.read without_header in
    Cstruct.hexdump without_header;
    (* let domain_length = Cstruct.get_uint8 without_header 0 in
    (* If last two bits are set (=192) then the packet is compressed! *)
    let is_compressed = phys_equal domain_length 192 in
    let questions = ref [] in
    if is_compressed
    then questions := decode_compressed_name without_header
    else (
      print_endline @@ string_of_bool is_compressed;
      print_endline @@ string_of_int domain_length;
      questions := "dd"); *)
    (* Cstruct.to_string without_header |> Format.printf "%s@"; *)
    { header; questions }
  ;;
end
