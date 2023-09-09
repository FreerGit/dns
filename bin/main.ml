open Core
open Eio
open Dns.Packet_buffer

let run_dgram2 ~e1 addr ~net sw ~buffer =
  let socket = Eio.Net.Ipaddr.V4.any in
  let server_addr = `Udp (socket, 8080) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  Fiber.both
    (fun () ->
      let buf = Cstruct.create 20 in
      traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp server_addr;
      let _, recv = Eio.Net.recv listening_socket buf in
      traceln "Received message %s" (Cstruct.to_string (Cstruct.sub buf 0 recv)))
    (fun () ->
      let e = Eio.Net.datagram_socket ~sw net e1 in
      traceln "Sending data to %a" Eio.Net.Sockaddr.pp addr;
      Eio.Net.send e ~dst:addr [ buffer ])
;;

let () =
  let open Dns.Protocol in
  (* let open Eio in *)
  let qname = "google.com" in
  let qtype = QueryType.A in
  (* let server = "8.8.8.8", 53 in *)
  (* let socket = `Udp (addr, 8081) in *)
  let header = DnsHeader.make () in
  let header = { header with id = 6666 } in
  let header = { header with questions = 1 } in
  let header = { header with recursion_desired = true } in
  let packet = DnsPacket.make header in
  let packet = { packet with questions = [ { name = qname; qtype } ] } in
  let req_buffer = PacketBuffer.create "" in
  DnsPacket.write req_buffer packet;
  (* Cstruct.to_string req_buffer.buf |> print_endline; *)
  (* print_endline "wtf"; *)
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let server =
    Ipaddr.V4.of_string_exn "8.8.8.8" |> Ipaddr.V4.to_octets |> Eio.Net.Ipaddr.of_raw
  in
  (* .Ipaddr.of_raw net "8.8.8.8" |> List.hd_exn in *)
  (* Eio.Net.Sockaddr.pp Format.std_formatter addr; *)
  (* Eio.Net.Ipaddr.pp Format.std_formatter addr; *)
  Eio.Switch.run (run_dgram2 (`Udp (server, 53)) ~e1:`UdpV4 ~net ~buffer:req_buffer.buf)
;;
