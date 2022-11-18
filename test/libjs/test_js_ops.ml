open Core
open Hardcaml

let arg width =
  let a = Bits.random ~width in
  let a' = Bits_list.Int_comb.of_constant (Bits.to_constant a) in
  a, a'
;;

let eq a a' =
  let a = Bits.bits_lsb a in
  let a' = List.rev a' in
  List.fold2_exn a a' ~init:true ~f:(fun acc a a' -> acc && Bits.to_int a = a')
;;

let test f f' num_tests max_bits =
  let width = 1 + Random.int max_bits in
  for _ = 0 to num_tests - 1 do
    let a, a' = arg width in
    let b, b' = arg width in
    let c = f a b in
    let c' = f' a' b' in
    if not (eq c c')
    then (
      let diff = Bits.of_constant (Bits_list.Int_comb.to_constant c') in
      raise_s [%message "mismatch" (a : Bits.t) (b : Bits.t) (c : Bits.t) (diff : Bits.t)])
  done
;;

let%expect_test "add" =
  for _ = 0 to 10 do
    test Bits.( +: ) Bits_list.Int_comb.( +: ) 10 1000
  done
;;

let%expect_test "sub" =
  for _ = 0 to 10 do
    test Bits.( -: ) Bits_list.Int_comb.( -: ) 10 1000
  done
;;

let test f f' num_tests max_bits =
  let width1 = 1 + Random.int max_bits in
  let width2 = 1 + Random.int max_bits in
  for i = 0 to num_tests - 1 do
    let a, a' = arg width1 in
    let b, b' = arg width2 in
    let c = f a b in
    let c' = f' a' b' in
    if not (eq c c')
    then (
      let diff = Bits.of_constant (Bits_list.Int_comb.to_constant c') in
      raise_s
        [%message
          "mismatch" (i : int) (a : Bits.t) (b : Bits.t) (c : Bits.t) (diff : Bits.t)])
  done
;;

let%expect_test "umul" =
  for _ = 0 to 10 do
    test Bits.( *: ) Bits_list.Int_comb.( *: ) 10 200
  done;
  [%expect
    {| |}]
;;

let%expect_test "smul" =
  for _ = 0 to 10 do
    test Bits.( *+ ) Bits_list.Int_comb.( *+ ) 10 200
  done;
  [%expect
    {| |}]
;;
