open Core

type t =
  { mutable header : Dns_header.t
  ; mutable questions : Dns_question.t list
  ; mutable answers : Dns_record.t list
  ; mutable authorities : Dns_record.t list
  ; mutable resources : Dns_record.t list
  }
[@@deriving show { with_path = false }]

let make header =
  { header; questions = []; answers = []; authorities = []; resources = [] }
;;

let read buffer =
  let header = Dns_header.read buffer in
  Dns_header.show header |> Eio.traceln "Read packet from query:\n %s \n %!";
  let packet =
    { header; questions = []; answers = []; authorities = []; resources = [] }
  in
  for _ = 1 to header.questions do
    let question = Dns_question.read buffer in
    packet.questions <- List.cons question packet.questions
  done;
  for _ = 1 to header.answers do
    let answer = Dns_record.read buffer in
    packet.answers <- List.cons answer packet.answers
  done;
  for _ = 1 to header.authoritative_entries do
    let authority = Dns_record.read buffer in
    packet.authorities <- List.cons authority packet.authorities
  done;
  for _ = 1 to header.resource_entries do
    let resource = Dns_record.read buffer in
    packet.resources <- List.cons resource packet.resources
  done;
  packet
;;

let write t buffer =
  t.header <- { t.header with questions = List.length t.questions };
  t.header <- { t.header with answers = List.length t.answers };
  t.header <- { t.header with authoritative_entries = List.length t.authorities };
  t.header <- { t.header with resource_entries = List.length t.resources };
  Dns_header.write buffer t.header;
  List.iter t.questions ~f:(Dns_question.write buffer);
  List.iter t.answers ~f:(fun req -> Dns_record.write buffer req |> ignore);
  List.iter t.authorities ~f:(fun req -> Dns_record.write buffer req |> ignore);
  List.iter t.resources ~f:(fun req -> Dns_record.write buffer req |> ignore);
  show t |> Eio.traceln "after write: \n %s \n";
  ()
;;

let get_random_a t =
  Iter.filter_map
    (fun record ->
      match record with
      | Dns_record.A { addr; _ } -> Some addr
      | _ -> None)
    (Iter.of_list t.answers)
  |> Iter.head
;;

let get_ns t qname =
  Iter.filter_map
    (fun record ->
      match record with
      | Dns_record.NS { domain; host; _ } -> Some (domain, host)
      | _ -> None)
    (Iter.of_list t.authorities)
  |> Iter.filter (fun (domain, _) -> String.is_suffix qname ~suffix:domain)
;;

let get_resolved_ns t qname =
  get_ns t qname
  |> Iter.flat_map (fun (_, host) ->
    Iter.filter_map
      (fun record ->
        match record with
        | Dns_record.A { domain; addr; _ } when String.( = ) domain host -> Some addr
        | _ -> None)
      (Iter.of_list t.resources))
  |> Iter.head
;;

let get_unresolved_ns t qname =
  get_ns t qname |> Iter.map (fun (_, host) -> host) |> Iter.head
;;
