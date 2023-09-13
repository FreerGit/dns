open Core

let%expect_test "DNS Header" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  Eio.Path.load path
  |> Dns.Packet_buffer.create
  |> Dns.Dns_header.read
  |> Dns.Dns_header.show
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
  |> Dns.Packet_buffer.create
  |> Dns.Dns_packet.read
  |> Dns.Dns_packet.show
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
