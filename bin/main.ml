open Core
open Eio
open Dns

let run_dgram2 addr ~net sw ~(buffer : Packet_buffer.t) =
  let socket = Eio.Net.Ipaddr.V4.any in
  let server_addr = `Udp (socket, 9090) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  let bytes = Cstruct.sub buffer.buf 0 buffer.pos in
  Eio.Net.send listening_socket ~dst:addr [ bytes ];
  let buf = Cstruct.create 512 in
  let _, recv = Eio.Net.recv listening_socket buf in
  Cstruct.sub buf 0 recv
;;

let ipv4_of_string ip =
  Ipaddr.V4.of_string_exn ip |> Ipaddr.V4.to_octets |> Eio.Net.Ipaddr.of_raw
;;

let lookup ~net qname qtype server =
  let header = Dns_header.make () in
  let header = { header with id = 6666 } in
  let header = { header with questions = 1 } in
  let header = { header with recursion_desired = true } in
  let packet = Dns.Dns_packet.make header in
  let packet = { packet with questions = [ { name = qname; qtype } ] } in
  let req_buffer = Packet_buffer.create "" in
  Dns.Dns_packet.write packet req_buffer;
  let resp = Eio.Switch.run @@ run_dgram2 (`Udp server) ~net ~buffer:req_buffer in
  let res_buffer = Packet_buffer.create @@ Cstruct.to_string resp in
  let res_packet = Dns_packet.read res_buffer in
  Ok res_packet
;;

let rec recursive_lookup ~ns ~net qname qtype =
  printf
    "Looking up %s %s with ns %s %!\n"
    (Query_type.show qtype)
    qname
    (Ipaddr.V4.to_string ns);
  let server = Ipaddr.V4.to_octets ns |> Net.Ipaddr.of_raw, 53 in
  let response = lookup ~net qname qtype server in
  match response with
  | Error e -> raise e
  | Ok res ->
    let entries_with_no_errors =
      (not (List.is_empty res.answers))
      |> ( && ) (phys_equal res.header.rescode Result_code.NOERROR)
    in
    let could_not_find_name = phys_equal res.header.rescode Result_code.NXDOMAIN in
    (match entries_with_no_errors || could_not_find_name with
     | true -> Ok res
     | false ->
       let find_new_ns = Dns_packet.get_resolved_ns res qname in
       (match find_new_ns with
        | Some new_ns -> recursive_lookup ~ns:new_ns ~net qname qtype
        | None ->
          (match Dns_packet.get_unresolved_ns res qname with
           | None -> Ok res
           | Some x ->
             let recursive_response =
               recursive_lookup ~ns ~net x Query_type.A |> Result.ok_exn
             in
             (match Dns_packet.get_random_a recursive_response with
              | Some new_ns -> recursive_lookup ~ns:new_ns ~net qname qtype
              | None -> Ok res))))
;;

let handle_query ~sw ~net udp_socket =
  let req_buffer = Packet_buffer.create "" in
  let listening_socket = Eio.Net.datagram_socket ~sw net udp_socket in
  let src, _ = Eio.Net.recv listening_socket req_buffer.buf in
  let request = Dns_packet.read req_buffer in
  let h = Dns_header.make () in
  let h = { h with id = request.header.id } in
  let h = { h with recursion_desired = true } in
  let h = { h with recursion_available = true } in
  let h = { h with query_response = true } in
  let packet = Dns_packet.make h in
  let question = List.hd request.questions in
  let packet =
    match question with
    | None ->
      { packet with header = { packet.header with rescode = Result_code.FORMERR } }
    | Some q ->
      let ns = Ipaddr.V4.of_string_exn "198.41.0.4" in
      (match recursive_lookup ~ns ~net q.name q.qtype with
       | Error e ->
         print_s [%message "%s" (Core.Exn.to_string e) ~loc:[%here]];
         { packet with header = { packet.header with rescode = Result_code.SERVFAIL } }
       | Ok res ->
         packet.questions <- List.cons q packet.questions;
         packet.header <- { packet.header with rescode = res.header.rescode };
         List.iter res.answers ~f:(fun record ->
           Dns_record.show record |> printf "%s %!\n";
           packet.answers <- List.cons record packet.answers);
         List.iter res.authorities ~f:(fun record ->
           Dns_record.show record |> printf "%s %!\n";
           packet.authorities <- List.cons record packet.authorities);
         List.iter res.resources ~f:(fun record ->
           Dns_record.show record |> printf "%s %!\n";
           packet.resources <- List.cons record packet.resources);
         packet)
  in
  let res_buffer = Packet_buffer.create "" in
  Dns_packet.write packet res_buffer;
  let len = res_buffer.pos in
  let data = Packet_buffer.get_range res_buffer ~pos:0 ~len in
  Eio.Net.send listening_socket ~dst:src [ data ]
;;

let () =
  let socket = `Udp (ipv4_of_string "0.0.0.0", 2053) in
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let rec loop () =
    Eio.Switch.run (fun sw -> handle_query ~net ~sw socket);
    loop ()
  in
  loop ()
;;
