type number = int
type plaintext = number
type ciphertext = number
type public_key = number * number
type private_key = number * number

let number s = int_of_string s
let string_of_number i = string_of_int i

(* --------------------------------------- *)
(* ----------- Implementations ----------- *)
(* --------------------------------------- *)

(* let hex_of_dec = function 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5' | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B' | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F';;
let dec_of_hex = function '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'A' -> 10 | 'B' -> 11 | 'C' -> 12 | 'D' -> 13 | 'E' -> 14 | 'F' -> 15;; *)

let rec power_int x n = 
  match n with
  | 0 -> 1
  | _ -> x * power_int x (n-1);;

let rec prime_check num factor sol = 
  if factor = num || sol = false then sol
  else 
    if num mod factor = 0 then prime_check num (factor+1) false
    else prime_check num (factor+1) sol;;

let rec find_primes ub index counter = 
  if index = 2 then  [2]
  else
    if counter > 0 then
      if (prime_check index 2 true) = true then [index]
        @ find_primes ub (index-1) (counter-1)
      else
        find_primes ub (index-1) counter
    else [];;

let rec check_gcd a b div = 
  if div >= a || div >= b then 
    if a mod div = 0 && b mod div = 0 then false
    else true
  else 
    if a mod div != 0 ||  b mod div != 0 then check_gcd a b (div+1)
    else false;;

let rec find_e phi_n e = 
  if e = phi_n then 1
  else 
    if (check_gcd phi_n e 2) = true then e
    else find_e phi_n (e+1);;

let rec find_d phi_n e d= 
  if (e*d) mod phi_n = 1 then d
  else find_d phi_n e (d+1);;

(* Testing keygen *)

let ub = 100;;
let n = List.nth (find_primes ub ub 2) (0) * List.nth (find_primes ub ub 2) (1);;
let phi_n =(((List.nth (find_primes ub ub 2) (0))-1) * ((List.nth (find_primes ub ub 2) (1))-1));;
let e = find_e phi_n 2;;
let d = (find_d phi_n e 1);;
(* print_endline("n = " ^ (Int.to_string n));;
print_endline("phi_n = " ^ (Int.to_string phi_n));;
print_endline("e = " ^ (Int.to_string e));;
print_endline("d = " ^ (Int.to_string d));; *)

(* -------------- *)

(* let rec dec_to_hex n = 
  if n / 16 = 0 then [hex_of_dec n]
  else [hex_of_dec (n mod 16) ] @ dec_to_hex (n/16);; *)

let rec char_list_to_string lst = 
  if List.length lst = 0 then ""
  else 
    (Char.escaped (List.hd lst)) ^ (char_list_to_string (List.tl lst));;

let rec addition a b carry = 
  if (List.length a) = 0 && (List.length b) = 0 then 
    if carry >= 1 then [carry]
    else []
  else 
    if (List.length a) != 0 && (List.length b) != 0 then
      [((List.hd a) + (List.hd b) + carry) mod 10] @ addition (List.tl a) (List.tl b) (((List.hd a) + (List.hd b) + carry)/10)
    else
      if (List.length a) == 0 then
        [((List.hd b) + carry) mod 10] @ addition a (List.tl b) (((List.hd b) + carry)/10)
      else
        [((List.hd a) + carry) mod 10] @ addition (List.tl a) b (((List.hd a) + carry)/10);;
        
(* print_endline("max array length = " ^ Int.to_string(Sys.max_array_length));;
print_endline("max int length = " ^ Int.to_string(Int.max_int));; *)

let num1 = [1; 2; 3; 4; 0; 0; 0];;
let num2 = [2; 4; 6; 8; 0; 0];;

let res = List.rev (addition (List.rev num1) (List.rev num2) 0);;

(* List.iter( fun x -> print_int(x) ) res;; *)

let rec multiplication_n_to_1 a n carry = 
  if List.length a = 0 then
    if carry >= 1 then [carry] else []
  else [((List.hd a) * n + carry) mod 10] @ multiplication_n_to_1 (List.tl a) n (((List.hd a) * n + carry)/10);;

let rec append_zeros z = 
  if z = 0 then []
  else [0] @ append_zeros (z-1);;

let rec multiplication_n_to_n a b digith = 
  if List.length b = 0 then [0]
  else addition (List.rev((List.rev(multiplication_n_to_1 (List.rev a) (List.hd b) 0)) @ (append_zeros digith))) (multiplication_n_to_n a (List.tl b) (digith+1)) 0;;
  (* else List.rev((List.rev(multiplication_n_to_1 (List.rev a) (List.hd b) 0)) @ (append_zeros digith));; *)

let num3 = [1;2;3;4];;
let num4 = [4;3;2;1];;
let lst = [1;2;3;4];;

let res = multiplication_n_to_n (num3) (List.rev lst) 0;;
let res0 = ((List.rev(multiplication_n_to_1 (List.rev num3) (List.hd (List.tl num4)) 0) @ (append_zeros 1)));;

let a = [0;0;0;4;3;2;1];;
let b = [0;0;8;6;4;2];;

let res4 = addition  (List.rev a)  (List.rev b) 0;;
(* print_endline("");;
List.iter( fun x -> print_int(x) ) (List.rev res);; *)

(* -- Exponent implementation -- *)

let rec power x n = 
  if n = 1 then (List.rev x)
  else multiplication_n_to_n x (power x (n-1)) 0;; 

let xa = [1;2;3;4];;

let res_exp = power xa 7;;
(* print_endline("");;
List.iter( fun x -> print_int(x) ) (List.rev res_exp);; *)

(* ----------------------------- *)


(* ------ Division and Mod ------ *)

let rec int_to_int_list num digith=
  if (power_int 10 (digith+1)) > num then [((num - (num/(power_int 10 (digith+1)))*( power_int 10 (digith+1))) - (num - (num/(power_int 10 digith))*(power_int 10 digith)))/(power_int 10 digith)]
  else 
    (int_to_int_list num (digith+1)) @ [((num - (num/(power_int 10 (digith+1)))*( power_int 10 (digith+1))) - (num - (num/(power_int 10 digith))*(power_int 10 digith)))/(power_int 10 digith)];;
  
let rec int_list_to_int num index = 
  if index + 1 >= List.length num then List.nth num(index) * (power_int 10 index)
  else 
    (List.nth num(index) * (power_int 10 index)) + (int_list_to_int num (index+1));;

let conversion = int_list_to_int [4;3;2;1;0] 0;;
(* print_endline("");;
print_endline("conversion = " ^ Int.to_string(conversion));; *)
(* List.iter( fun x -> print_int(x) ) conversion;; *)

let rec division num remainder divisor quotient = 
  if (List.length num = 0) && (int_list_to_int (List.rev remainder) 0) < divisor then [ (int_list_to_int (List.rev remainder) 0); (int_list_to_int (List.rev quotient) 0)]
  else
    if (int_list_to_int (List.rev remainder) 0) >= divisor then (division num (int_to_int_list ((int_list_to_int (List.rev remainder) 0)  mod divisor) 0) divisor (quotient @ [(int_list_to_int (List.rev remainder) 0)  / divisor]))
    else (division (List.tl num) (remainder @ [(List.hd num)]) divisor quotient);;

let division_test = division (List.rev res_exp) [0] 3599 [];;

(* print_endline(" 1522756 / 3599 = " ^ Int.to_string(List.nth division_test(1)));;
print_endline(" 1522756 mod 3599 = " ^ Int.to_string(List.nth division_test(0)));; *)

let cipher_txt = division (List.rev(power (int_to_int_list (12345) 0) 5)) [0] 14351 [];;
(* print_endline("ciphertext for m = 12345 is " ^ Int.to_string(List.nth cipher_txt(0)));; *)

(* ------------------------------ *)

(* print_endline("1056 = " ^ char_list_to_string(List.rev (dec_to_hex 1056)));; *)

(* let rec hex_to_dec lst =  *)


(* let rec hex_addition a b carry =  *)


(* -- Testing enc -- *)

let m = 1234;;

let rec power_mod (exp, modulus) base res = 
  if exp <= 0 then res
  else 
    if exp mod 2 = 1 then (power_mod  ((Float.to_int(Float.floor(Float.div (Int.to_float(exp)) 2.))), modulus) (base * base mod modulus) (res * base mod modulus))
    else (power_mod ((Float.to_int(Float.floor(Float.div (Int.to_float(exp)) 2.))), modulus) (base * base mod modulus) res);;

let rec powermod (exp, modulus) base res =
  if exp = 0 || base = 0 then
    if base = 0 then 0
    else 1
  else  
    if exp mod 2 = 0 then
      ((((powermod (exp/2, modulus) base 1) * (powermod (exp/2, modulus) base 1)) mod modulus) + modulus) mod modulus
    else 
      ((((base mod modulus) * (powermod ((exp-1), modulus) base 1)) mod modulus) + modulus) mod modulus;;

(* let powermod_test = power_mod 12 5069 8633 1;; *)

(* let rec power_mod base exp modulus =
  if (power_int (base mod modulus) exp) < (Int.max_int / 1000) && (power_int base exp) > 0 then (power_int (base mod modulus) exp) mod modulus
  else
    if base < modulus then  power_mod (base *base) (exp-1) modulus
    else
      power_mod (base mod modulus) exp modulus;; *)
(* let powermod_test = power_mod 12 5069 8633;; *)


(* print_endline("12^5069 mod 8633 = " ^ Int64.to_string(Int64.max_int));; *)

(* let c = (power m e) mod n;; *)
(* print_endline("m = " ^ (Int.to_string m));;
print_endline("c = " ^ (Int.to_string c));; *)
(* ----------------- *)

let rec cracking e n c res = 
  if (power_mod (e, n) res 1) = c then res
  else cracking e n c (res+1);;

(* --------------------------------------- *)
(* ----------------- END ----------------- *)
(* --------------------------------------- *)

let keygen ub =  
  let n = List.nth (find_primes ub ub 2) (0) * List.nth (find_primes ub ub 2) (1) in
  let phi_n = (((List.nth (find_primes ub ub 2) (0))-1) * ((List.nth (find_primes ub ub 2) (1))-1)) in
  let e = (find_e phi_n 2) in
  (e, n), ((find_d phi_n e 1), n);;
(* let keygen ub = (5, 8633), (5069,8633);; *)
let enc (e, n) m = List.nth (division (List.rev(power (int_to_int_list (m) 0) e)) [0] n []) (0);;
(* let enc (e,n) m = powermod m e n 1;; *)
(* let dec (d, n) c = power_mod (d, n) c 1;; *)
let dec (d, n) c = power_mod (d, n) c 1;;
let crack (e, n) c = cracking e n c 1;;

let rec prime_check num factor sol = 
  if factor = num || sol = false then sol
  else 
    if num mod factor = 0 then prime_check num (factor+1) false
    else prime_check num (factor+1) sol;;

let num = 98;;




(* if (prime_check num 2 true) = true then print_endline(Int.to_string(num) ^ " is a prime")
else print_endline(Int.to_string(num) ^ " is not a prime");; *)


let rec find_primes ub index counter = 
  if index = 2 then  [2]
  else
    if counter > 0 then
      if (prime_check index 2 true) = true then [index]
        @ find_primes ub (index-1) (counter-1)
      else
        find_primes ub (index-1) counter
    else [];;

(* let ub = 100;;
let res = (List.nth (find_primes ub ub 2) (1));;
print_int (res); *)