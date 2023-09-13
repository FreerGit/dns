
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
    ; rescode : Result_code.t
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
      ; rescode = Result_code.NOERROR
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
    let id = Packet_buffer.read_u16 buffer in
    let flags = Packet_buffer.read_u16 buffer in
    let a = flags lsr 8 in
    let b = flags land 0xFF in
    let questions = Packet_buffer.read_u16 buffer in
    let answers = Packet_buffer.read_u16 buffer in
    let authoritative_entries = Packet_buffer.read_u16 buffer in
    let resource_entries = Packet_buffer.read_u16 buffer in
    let result : t =
      { id
      ; recursion_desired = a land (1 lsl 0) > 0
      ; truncated_message = a land (1 lsl 1) > 0
      ; authoritative_answer = a land (1 lsl 2) > 0
      ; opcode = (a lsr 3) land 0x0F
      ; query_response = a land (1 lsl 7) > 0
      ; rescode = Result_code.of_t (b land 0x0F)
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
    Packet_buffer.write_u16 buffer t.id;
    let to_int = Bool.to_int in
    Packet_buffer.write
      buffer
      (to_int t.recursion_desired
       lor (to_int t.truncated_message lsl 1)
       lor (to_int t.authoritative_answer lsl 2)
       lor (t.opcode lsl 3)
       lor (to_int t.query_response lsl 7));
    Packet_buffer.write
      buffer
      (Result_code.to_int t.rescode
       lor (to_int t.checking_disabled lsl 4)
       lor (to_int t.authed_data lsl 5)
       lor (to_int t.z lsl 6)
       lor (to_int t.recursion_available lsl 7));
    Packet_buffer.write_u16 buffer t.questions;
    Packet_buffer.write_u16 buffer t.answers;
    Packet_buffer.write_u16 buffer t.authoritative_entries;
    Packet_buffer.write_u16 buffer t.resource_entries
  ;;
