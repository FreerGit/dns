open Core

let%expect_test "DNS Header" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  Eio.Path.load path
  |> Cstruct.of_string
  |> Dns.Protocol.DnsHeader.read
  |> Dns.Protocol.DnsHeader.show
  |> print_endline;
  [%expect
    {|
    { id = 34346; query_response = true; opcode = 0;
      authoritative_answer = false; truncated_message = false;
      recursion_desired = true; recursion_available = true; z = 0;
      rescode = NOERROR; questions = 1; answers = 1; authoritative_entries = 0;
      resource_entries = 0 }|}]
;;

let%expect_test "DNS Packet" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  Eio.Path.load path
  |> Cstruct.of_string
  |> Dns.Protocol.DnsPacket.read
  |> Dns.Protocol.DnsPacket.show
  |> print_endline;
  [%expect
    {|
    { header =
      { id = 34346; query_response = true; opcode = 0;
        authoritative_answer = false; truncated_message = false;
        recursion_desired = true; recursion_available = true; z = 0;
        rescode = NOERROR; questions = 1; answers = 1; authoritative_entries = 0;
        resource_entries = 0 };
      questions = [{ name = "google.com"; qtype = A }] }|}]
;;
