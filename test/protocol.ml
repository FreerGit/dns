open Core


let%expect_test "DNS Packet" =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run
  @@ fun env ->
  let path = Eio.Stdenv.cwd env / "packets/response_packet.txt" in
  (* Eio.traceln ~__POS__ "%a" Eio.Path.pp path; *)
  (* Eio.Path.with_lines path (fun lines -> Seq.iter (Eio.traceln "Processing %S") lines); *)
  Eio.Path.load path |> Bytes.of_string |>
  Dns.Protocol.DnsHeader.read |> Dns.Protocol.DnsHeader.show |> Eio.traceln "%S";
  (* Eio.Stdenv.cwd *)
  printf "%d" (1 + 2);
  [%expect {| 3 |}]
;;
