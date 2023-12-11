let usage =
  "Usage: Run one of the following commands:\n\
  \    rsa keygen [ upper bound ]\n\
  \    rsa enc [ public key: e ] [ public key: n ] [ plain message ]\n\
  \    rsa dec [ private key: d ] [ private key: n ] [ cipher message ]\n\
  \    rsa crack [ public key: e ] [ public key: n ] [ cipher message ]"

let main argv =
  let map f (p, q) = (f p, f q) in
  match argv.(1) with
  | "keygen" ->
      let public_key, private_key = argv.(2) |> Rsa.number |> Rsa.keygen in
      let e, pub_n = map Rsa.string_of_number public_key in
      print_endline e;
      print_endline pub_n;
      let d, prv_n = map Rsa.string_of_number private_key in
      print_endline d;
      print_endline prv_n
  | "enc" ->
      let public_key = map Rsa.number (argv.(2), argv.(3)) in
      let plain_text = Rsa.number argv.(4) in
      Rsa.enc public_key plain_text |> Rsa.string_of_number |> print_endline
  | "dec" ->
      let private_key = map Rsa.number (argv.(2), argv.(3)) in
      let cipher_text = Rsa.number argv.(4) in
      Rsa.dec private_key cipher_text |> Rsa.string_of_number |> print_endline
  | "crack" ->
      let public_key = map Rsa.number (argv.(2), argv.(3)) in
      let cipher_text = Rsa.number argv.(4) in
      Rsa.crack public_key cipher_text |> Rsa.string_of_number |> print_endline
  | _ -> failwith "invalid command"

let _ = main Sys.argv
