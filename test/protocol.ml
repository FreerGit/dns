open Core
open Dns.Packet_buffer

let%expect_test "DNS Header" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  Eio.Path.load path
  (* |> Cstruct.of_string *)
  |> PacketBuffer.create
  |> Dns.Protocol.DnsHeader.read
  |> Dns.Protocol.DnsHeader.show
  |> print_endline;
  [%expect
    {|
    { id = 34346; recursion_desired = true; truncated_message = false;
      authoritative_answer = false; opcode = 0; query_response = true;
      rescode = NOERROR; checking_disabled = false; authed_data = false;
      z = false; recursion_available = true; questions = 1; answers = 1;
      authoritative_entries = 0; resource_entries = 0 }|}]
;;

let%expect_test "DNS Packet" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  Eio.Path.load path
  |> PacketBuffer.create
  |> Dns.Protocol.DnsPacket.read
  |> Dns.Protocol.DnsPacket.show
  |> print_endline;
  [%expect
    {|
    { header =
      { id = 34346; recursion_desired = true; truncated_message = false;
        authoritative_answer = false; opcode = 0; query_response = true;
        rescode = NOERROR; checking_disabled = false; authed_data = false;
        z = false; recursion_available = true; questions = 1; answers = 1;
        authoritative_entries = 0; resource_entries = 0 };
      questions = [{ name = "google.com"; qtype = A }];
      answers = [A {domain = "google.com"; addr = 216.58.211.142; ttl = 293}];
      authorities = []; resources = [] }|}]
;;

let run_dgram2 ~e1 addr ~net sw =
  let open Eio in
  let server_addr = `Udp (addr, 8082) in
  let listening_socket = Eio.Net.datagram_socket ~sw net server_addr in
  Fiber.both
    (fun () ->
      let buf = Cstruct.create 20 in
      traceln "Waiting to receive data on %a" Eio.Net.Sockaddr.pp server_addr;
      let _, recv = Eio.Net.recv listening_socket buf in
      traceln "Received message %s" Cstruct.(to_string (sub buf 0 recv)))
    (fun () ->
      let e = Eio.Net.datagram_socket ~sw net e1 in
      traceln "Sending data to %a" Eio.Net.Sockaddr.pp server_addr;
      Eio.Net.send e ~dst:server_addr [ Cstruct.of_string "UDP Message" ])
;;

(* let%expect_test "Resolver" =
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
  let req_buffer = PacketBuffer.create (Bytes.create 0) in
  DnsPacket.write req_buffer packet;
  let b_buffer = req_buffer.buf in
  let buffer = Buffer.create 512 in
  Bigbuffer.contents_bytes b_buffer
  |> fun b_buff ->
  Buffer.add_bytes buffer b_buff;
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let server =
    Ipaddr.V4.of_string_exn "8.8.8.8" |> Ipaddr.V4.to_octets |> Eio.Net.Ipaddr.of_raw
  in
  (* let socket = Eio.Net.Ipaddr.V4.any in *)
  (* .Ipaddr.of_raw net "8.8.8.8" |> List.hd_exn in *)
  (* Eio.Net.Sockaddr.pp Format.std_formatter addr; *)
  (* Eio.Net.Ipaddr.pp Format.std_formatter addr; *)
  Eio.Switch.run (run_dgram2 server ~e1:`UdpV4 ~net);
  [%expect {||}]
;; *)
