open Core

module ResultCode = struct
  type t =
    | NOERROR
    | FORMERR
    | SERVFAIL
    | NXDOMAIN
    | NOTIMP
    | REFUSED
  [@@deriving bin_io]

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
  [@@deriving bin_io]
end

module DnsPacket = struct
  type t = { header : DnsHeader.t } [@@deriving bin_io]
end
