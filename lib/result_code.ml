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
