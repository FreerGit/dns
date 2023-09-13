type t =
  | UNKOWN of int
  | A
  | NS
  | CNAME
  | MX
  | AAAA
[@@deriving show { with_path = false }]

let num_of_t t =
  match t with
  | UNKOWN x -> x
  | A -> 1
  | NS -> 2
  | CNAME -> 5
  | MX -> 15
  | AAAA -> 28
;;

let t_of_num num =
  match num with
  | 1 -> A
  | 2 -> NS
  | 5 -> CNAME
  | 15 -> MX
  | 28 -> AAAA
  | _ -> UNKOWN num
;;
