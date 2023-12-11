open OUnit2
open Rsa

let map f (p, q) = (f p, f q)

let print_vanilla_key pair =
  let p, q = map string_of_number pair in
  "[" ^ p ^ "," ^ q ^ "]"

let encode_vanilla_key pair = map string_of_int pair |> map number

let vanilla_1 _ =
  let pub_key, prv_key = number "64" |> keygen in
  assert_equal ~msg:"vanilla_1:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (7, 3599))
    pub_key;
  assert_equal ~msg:"vanilla_1:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (2983, 3599))
    prv_key;
  let m = number "1234" in
  let c = number "1112" in
  assert_equal ~msg:"vanilla_1:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_1:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_1:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_2 _ =
  let pub_key, prv_key = number "128" |> keygen in
  assert_equal ~msg:"vanilla_2:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (5, 14351))
    pub_key;
  assert_equal ~msg:"vanilla_2:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (5645, 14351))
    prv_key;
  let m = number "12345" in
  let c = number "2050" in
  assert_equal ~msg:"vanilla_2:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_2:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_2:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_3 _ =
  let pub_key, prv_key = number "256" |> keygen in
  assert_equal ~msg:"vanilla_3:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (7, 60491))
    pub_key;
  assert_equal ~msg:"vanilla_3:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (17143, 60491))
    prv_key;
  let m = number "12345" in
  let c = number "60128" in
  assert_equal ~msg:"vanilla_3:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_3:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_3:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_4 _ =
  let pub_key, prv_key = number "512" |> keygen in
  assert_equal ~msg:"vanilla_4:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (3, 256027))
    pub_key;
  assert_equal ~msg:"vanilla_4:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (170011, 256027))
    prv_key;
  let m = number "123456" in
  let c = number "249472" in
  assert_equal ~msg:"vanilla_4:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_4:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_4:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_5 _ =
  let pub_key, prv_key = number "1024" |> keygen in
  assert_equal ~msg:"vanilla_5:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (7, 1040399))
    pub_key;
  assert_equal ~msg:"vanilla_5:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (890023, 1040399))
    prv_key;
  let m = number "123456" in
  let c = number "580676" in
  assert_equal ~msg:"vanilla_5:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_5:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_5:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_6 _ =
  let pub_key, prv_key = number "2048" |> keygen in
  assert_equal ~msg:"vanilla_6:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (5, 4137131))
    pub_key;
  assert_equal ~msg:"vanilla_6:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (826613, 4137131))
    prv_key;
  let m = number "1234567" in
  let c = number "2628331" in
  assert_equal ~msg:"vanilla_6:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_6:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_6:crack" ~printer:string_of_number m
    (crack pub_key c)

let vanilla_7 _ =
  let pub_key, prv_key = number "32768" |> keygen in
  assert_equal ~msg:"vanilla_7:keygen:public_key" ~printer:print_vanilla_key
    (encode_vanilla_key (5, 1071514531))
    pub_key;
  assert_equal ~msg:"vanilla_7:keygen:private_key" ~printer:print_vanilla_key
    (encode_vanilla_key (214289813, 1071514531))
    prv_key;
  let m = number "536870912" in
  let c = number "41961153" in
  assert_equal ~msg:"vanilla_7:enc" ~printer:string_of_number c (enc pub_key m);
  assert_equal ~msg:"vanilla_7:dec" ~printer:string_of_number m (dec prv_key c);
  assert_equal ~msg:"vanilla_7:crack" ~printer:string_of_number m
    (crack pub_key c)

let suite =
  "suite"
  >::: [
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_1);
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_2);
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_3);
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_4);
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_5);
         OUnitTest.TestCase (OUnitTest.Custom_length 5., vanilla_6);
         (* OUnitTest.TestCase (OUnitTest.Custom_length 60., vanilla_7); *)
       ]

let _ = OUnit2.run_test_tt_main suite
