open Core
open Eio
open Dns.Packet_buffer

let run_dgram2 addr ~net sw ~buffer =
  let socket = Eio.Net.Ipaddr.V4.any in
  let server_addr = `Udp (socket, 8080) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  Fiber.fork ~sw (fun () ->
    (* let e = Eio.Net.datagram_socket ~sw net e1 in *)
    (* traceln "Sending data to %a" Eio.Net.Sockaddr.pp addr; *)
    Eio.Net.send listening_socket ~dst:addr [ buffer ]);
  Fiber.fork_promise ~sw (fun () ->
    let buf = Cstruct.create 500 in
    (* traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp server_addr; *)
    let _, recv = Eio.Net.recv listening_socket buf in
    (* traceln "Received message %s" (Cstruct.to_hex_string (Cstruct.sub buf 0 recv)); *)
    Cstruct.sub buf 0 recv)
;;

let () =
  let open Dns.Protocol in
  let qname = "www.yahoo.com" in
  let qtype = QueryType.A in
  let header = DnsHeader.make () in
  let header = { header with id = 6666 } in
  let header = { header with questions = 1 } in
  let header = { header with recursion_desired = true } in
  let packet = DnsPacket.make header in
  let packet = { packet with questions = [ { name = qname; qtype } ] } in
  let req_buffer = PacketBuffer.create "" in
  DnsPacket.write req_buffer packet;
  let x =
    Eio_main.run
    @@ fun env ->
    let net = Eio.Stdenv.net env in
    let server =
      Ipaddr.V4.of_string_exn "8.8.8.8" |> Ipaddr.V4.to_octets |> Eio.Net.Ipaddr.of_raw
    in
    Eio.Switch.run (run_dgram2 (`Udp (server, 53)) ~net ~buffer:req_buffer.buf)
  in
  let y = Promise.await_exn x in
  let res_packet = DnsPacket.read (PacketBuffer.create @@ Cstruct.to_string y) in
  DnsPacket.show res_packet |> print_endline
;;
