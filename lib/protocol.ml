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
    ; recursion_desired : uint8_t
    ; truncated_message : uint8_t [@len 1]
    (* 
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
    ; resource_entries : uint16_t *)
    }
  [@@big_endian]]

  type t =
    { id : int
    ; recursion_desired : string
    (* 
    ; response : char 
    ; truncated_message : bool
    ; authoritative_answer : bool
    ; opcode : int
    *)
    (* ; rescode : ResultCode.t *)
    (* ; checking_disabled : bool
    ; authed_data : bool
    ; z : bool
    ; recursion_available : bool
    ; questions : int
    ; answers : int
    ; authoritative_entries : int
    ; resource_entries : int *)
    } [@@deriving show]

    let read bytes = 
      (* Eio.traceln "%S" bytes; *)
      let idk = Cstruct.of_bytes bytes in

      let result: t = {
        id = get_t_id idk
        ; recursion_desired = get_t_recursion_desired idk |> string_of_int 
      }
    in result
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
  type t = {
    name: string
    ; qtype: QueryType.t
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
  | UNKOWN of {
    domain : string
    ; qtype: int
    ; data_len : int
    ; ttl: int
  }
  | A of {
    domain: string
    ; addr: string
    ; ttl: int
  }
end

module DnsPacket = struct
  type t = { 
    header : DnsHeader.t 
  }
end
