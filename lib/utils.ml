open Core

let bool_of_int n =
  match n with
  | 0 -> false
  | 1 -> true
  | otherwise ->
    raise_s [%message "error" (sprintf "Not a boolean, but: %d" otherwise) ~loc:[%here]]
;;
