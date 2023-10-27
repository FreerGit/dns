open Core
open Eio
open Dns
open Async

let run_dgram2 to_addr (buffer : Packet_buffer.t) =
  let port = 43210 in
  let addr = Socket.Address.Inet.create (Core_unix.Inet_addr.of_string "0.0.0.0") ~port in
  let socket = Async_udp.bind addr in
  let socket = Socket.fd socket in
  let sendto = Or_error.ok_exn @@ Async_udp.sendto () (* addr *) in
  traceln "%s" @@ string_of_int buffer.pos;
  let f =
    sendto
      socket
      (buffer.buf |> Cstruct.to_string ~off:0 ~len:buffer.pos |> Iobuf.of_string)
      to_addr
  in
  Deferred.upon f (fun _ -> print_endline "sent");
  let buf = ref @@ "" in
  let%bind _ =
    Async_udp.recvfrom_loop socket (fun bufs adr ->
      traceln "%s" @@ Socket.Address.Inet.to_string adr;
      traceln "recv";
      buf := Iobuf.to_string bufs;
      don't_wait_for @@ Fd.close socket)
  in
  traceln "here";
  traceln "%s" @@ string_of_int @@ String.length !buf;
  return @@ Cstruct.of_string !buf
;;

let lookup _addr qname qtype server =
  let header = Dns_header.make () in
  let header = { header with id = 6666 } in
  let header = { header with questions = 1 } in
  let header = { header with recursion_desired = true } in
  let packet = Dns.Dns_packet.make header in
  let packet = { packet with questions = [ { name = qname; qtype } ] } in
  let req_buffer = Packet_buffer.create "" in
  Dns.Dns_packet.write packet req_buffer;
  let%bind resp = run_dgram2 server req_buffer in
  traceln "resp: ";
  Cstruct.hexdump resp;
  traceln "resp: ";
  let res_buffer = Packet_buffer.create @@ Cstruct.to_string resp in
  let res_packet = Dns_packet.read res_buffer in
  return @@ Ok res_packet
;;

(* res_packet *)

let rec recursive_lookup ns_addr addr qname qtype =
  let server = ns_addr in
  traceln "Looking up %s %s with ns %s" (Query_type.show qtype) qname ns_addr;
  let server =
    Socket.Address.Inet.create ~port:53 (Core_unix.Inet_addr.of_string server)
  in
  let%bind response = lookup addr qname qtype server in
  match response with
  | Error e -> raise e
  | Ok res ->
    traceln "new ns name %s" qname;
    let entries_with_no_errors =
      (not (List.is_empty res.answers))
      |> ( && ) (phys_equal res.header.rescode Result_code.NOERROR)
    in
    let could_not_find_name = phys_equal res.header.rescode Result_code.NXDOMAIN in
    (match entries_with_no_errors || could_not_find_name with
     | true -> return @@ Ok res
     | false ->
       let find_new_ns = Dns_packet.get_resolved_ns res qname in
       (match find_new_ns with
        | Some new_ns ->
          recursive_lookup (Ipaddr.V4.to_string new_ns) addr qname Query_type.A
        | None ->
          (match Dns_packet.get_unresolved_ns res qname with
           | None -> return @@ Ok res
           | Some x ->
             let%bind recursive_response = recursive_lookup ns_addr addr x Query_type.A in
             (match Dns_packet.get_random_a (recursive_response |> Result.ok_exn) with
              | Some new_ns ->
                recursive_lookup (Ipaddr.V4.to_string new_ns) addr qname qtype
              | None -> return @@ Ok res))))
;;

let handle_query addr buffer =
  let request = Dns_packet.read buffer in
  let h = Dns_header.make () in
  let h = { h with id = request.header.id } in
  let h = { h with recursion_desired = true } in
  let h = { h with recursion_available = true } in
  let h = { h with query_response = true } in
  let packet = Dns_packet.make h in
  let question = List.hd request.questions in
  let%bind packet =
    match question with
    | None ->
      return
      @@ { packet with header = { packet.header with rescode = Result_code.FORMERR } }
    | Some q ->
      traceln "Recieved query: %s" @@ Dns_question.show q;
      let%bind rlookup = recursive_lookup "192.33.4.12" addr q.name q.qtype in
      (match rlookup with
       | Error e ->
         print_s [%message "%s" (Core.Exn.to_string e) ~loc:[%here]];
         return
         @@ { packet with header = { packet.header with rescode = Result_code.SERVFAIL } }
       | Ok res ->
         packet.questions <- List.cons q packet.questions;
         packet.header <- { packet.header with rescode = res.header.rescode };
         List.iter res.answers ~f:(fun record ->
           packet.answers <- List.cons record packet.answers);
         List.iter res.authorities ~f:(fun record ->
           packet.authorities <- List.cons record packet.authorities);
         List.iter res.resources ~f:(fun record ->
           packet.resources <- List.cons record packet.resources);
         return @@ packet)
  in
  let res_buffer = Packet_buffer.create "" in
  Dns_packet.write packet res_buffer;
  let port = 44444 in
  let addr = Socket.Address.Inet.create Core_unix.Inet_addr.localhost ~port in
  let socket = Async_udp.bind addr in
  let socket = Socket.fd socket in
  let sendto = Or_error.ok_exn @@ Async_udp.sendto () (* addr *) in
  sendto socket (res_buffer.buf |> Cstruct.to_string |> Iobuf.of_string) addr
;;

let dns_server () =
  let port = 2053 in
  let addr = Socket.Address.Inet.create Core_unix.Inet_addr.localhost ~port in
  let socket = Async_udp.bind addr in
  let socket = Socket.fd socket in
  let stop = Async.never () in
  let config = Async_udp.Config.create ~stop () in
  let _ =
    Async_udp.recvfrom_loop ~config socket (fun bufs adr ->
      traceln "recv dns loop";
      let buffer = Iobuf.to_string bufs |> Packet_buffer.create in
      don't_wait_for (handle_query adr buffer))
  in
  Deferred.never ()
;;

let () =
  Command.async ~summary:"An echo server" (Command.Param.return dns_server)
  |> Command_unix.run
;;
