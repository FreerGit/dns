open Core

module ResultCode = struct
  type t =
    | NOERROR
    | FORMERR
    | SERVFAIL
    | NXDOMAIN
    | NOTIMP
    | REFUSED

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
    ; recursion_desired : char_t
    ; truncated_message : char_t
    ; authoritative_answer : char_t
    ; opcode : uint8_t [@len 4]
    ; response : char_t
    ; rescode : uint8_t
    ; checking_disabled : char_t
    ; authed_data : char_t
    ; z : char_t
    ; recursion_available : char_t
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
    ; response : bool
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
end

module DnsPacket = struct
  type t = { header : DnsHeader.t }
end
