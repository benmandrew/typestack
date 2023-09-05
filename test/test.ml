open Typestack
open Ppx_compare_lib.Builtin
open Sexplib.Std

let%test_unit "Typecheck Completeness" =
  let open Main in
  let e = Seq (Val (N 0), Seq (Val (N 1), Add)) in
  let result = Type_check.v e [] in
  [%test_result: bool] result ~expect:true

let%test_unit "Typecheck Soundness" =
  let open Main in
  let e = Seq (Val (N 0), Seq (Val (B false), Add)) in
  let result = Type_check.v e [] in
  [%test_result: bool] result ~expect:false
