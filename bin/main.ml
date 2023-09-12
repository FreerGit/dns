open Core
open Eio
open Dns.Packet_buffer
open Dns.Protocol

let run_dgram2 addr ~net sw ~buffer =
  let socket = Eio.Net.Ipaddr.V4.any in
  let server_addr = `Udp (socket, 8080) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  Fiber.fork ~sw (fun () -> Eio.Net.send listening_socket ~dst:addr [ buffer ]);
  Fiber.fork_promise ~sw (fun () ->
    let buf = Cstruct.create 500 in
    let _, recv = Eio.Net.recv listening_socket buf in
    Cstruct.sub buf 0 recv)
;;

let broadcast_of_string ip =
  Ipaddr.V4.of_string_exn ip |> Ipaddr.V4.to_octets |> Eio.Net.Ipaddr.of_raw
;;

let lookup qname qtype =
  let header = DnsHeader.make () in
  let header = { header with id = 6666 } in
  let header = { header with questions = 1 } in
  let header = { header with recursion_desired = true } in
  let packet = DnsPacket.make header in
  let packet = { packet with questions = [ { name = qname; qtype } ] } in
  let req_buffer = PacketBuffer.create "" in
  DnsPacket.write req_buffer packet;
  let resp =
    Eio_main.run
    @@ fun env ->
    let net = Eio.Stdenv.net env in
    let server = broadcast_of_string "8.8.8.8" in
    Eio.Switch.run (run_dgram2 (`Udp (server, 53)) ~net ~buffer:req_buffer.buf)
  in
  let data = Eio.Promise.await resp in
  match data with
  | Error e -> Error (Core.Exn.to_string e)
  | Ok c ->
    let res_packet = DnsPacket.read (PacketBuffer.create @@ Cstruct.to_string c) in
    Ok res_packet
;;

let handle_query ~sw ~net udp_socket =
  let req_buffer = PacketBuffer.create "" in
  let listening_socket = Eio.Net.datagram_socket ~sw net udp_socket in
  let src, _ = Eio.Net.recv listening_socket req_buffer.buf in
  let request = DnsPacket.read req_buffer in
  let h = DnsHeader.make () in
  let h = { h with id = request.header.id } in
  let h = { h with recursion_desired = true } in
  let h = { h with recursion_available = true } in
  let h = { h with query_response = true } in
  let packet = DnsPacket.make h in
  let question = List.hd request.questions in
  let packet =
    match question with
    | None -> { packet with header = { packet.header with rescode = ResultCode.FORMERR } }
    | Some q ->
      (match lookup q.name q.qtype with
       | Error e ->
         print_s [%message "" (sprintf "%s" e) ~loc:[%here]];
         { packet with header = { packet.header with rescode = ResultCode.SERVFAIL } }
       | Ok res ->
         packet.questions <- List.cons q packet.questions;
         packet.header <- { packet.header with rescode = res.header.rescode };
         List.iter res.answers ~f:(fun record ->
           packet.answers <- List.cons record packet.answers);
         List.iter res.authorities ~f:(fun record ->
           packet.authorities <- List.cons record packet.authorities);
         List.iter res.resources ~f:(fun record ->
           packet.resources <- List.cons record packet.resources);
         packet)
  in
  let res_buffer = PacketBuffer.create "" in
  DnsPacket.write res_buffer packet;
  let len = res_buffer.pos in
  let data = PacketBuffer.get_range res_buffer ~pos:0 ~len in
  Eio.Net.send listening_socket ~dst:src [ data ]
;;

let () =
  let socket = `Udp (broadcast_of_string "0.0.0.0", 2053) in
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let rec loop () =
    Eio.Switch.run (fun sw -> handle_query ~net ~sw socket);
    loop ()
  in
  loop ()
;;
