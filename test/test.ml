open Typestack.Main
open Ppx_compare_lib.Builtin
open Sexplib.Std

let typecheck_test ~expect e =
  let result = Type_check.v e [] in
  [%test_result: bool] result ~expect

let exec_test ~expect e =
  if not (Type_check.v e []) then failwith "Expression does not typecheck!";
  let result = Exec.v e in
  [%test_result: stack] result ~expect

let%test_unit "typecheck completeness" =
  let e = Seq (Val (N 0), Seq (Val (N 1), Seq (Add, Print))) in
  typecheck_test ~expect:true e

let%test_unit "typecheck soundness" =
  let e = Seq (Val (N 0), Seq (Val (B false), Seq (Add, Print))) in
  typecheck_test ~expect:false e;
  let e = Seq (Val (B false), Cond (Val (N 1), Val (B true))) in
  typecheck_test ~expect:false e

let%test_unit "exec add" =
  let e = Seq (Val (N 2), Seq (Val (N 1), Add)) in
  exec_test ~expect:[ N 3 ] e

let%test_unit "exec cond" =
  let e = Seq (Val (N 9), Seq (Val (B false), Cond (Val (N 3), Val (N 5)))) in
  exec_test ~expect:[ N 5; N 9 ] e
