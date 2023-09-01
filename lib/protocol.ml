open Core

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
    | UNKNOWN of int
    | A
end

module DnsQuestion = struct
  type t =
    { name : string
    ; qtype : QueryType.t
    }
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
  type t = { header : DnsHeader.t }
end
