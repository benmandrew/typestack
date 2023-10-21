open Typestack
open Ppx_compare_lib.Builtin
open Sexplib.Std

let typecheck_test ~expect e =
  let result = Typecheck.v e [] in
  [%test_result: bool] result ~expect

let exec_test ~expect e =
  if not (Typecheck.v e []) then failwith "Expression does not typecheck!";
  let result = Exec.v e in
  [%test_result: T.v_stack] result ~expect

let%test_unit "typecheck completeness" =
  let e = T.Seq (Val (N 0), Seq (Val (N 1), Add)) in
  typecheck_test ~expect:true e

let%test_unit "typecheck soundness" =
  let e = T.Seq (Val (N 0), Seq (Val (B false), Add)) in
  typecheck_test ~expect:false e;
  let e = T.Seq (Val (B false), Cond (Val (N 1), Val (B true))) in
  typecheck_test ~expect:false e;
  let e = T.Cond (Add, Add) in
  typecheck_test ~expect:false e

let%test_unit "exec" =
  let e = T.Seq (Val (N 2), Seq (Val (N 1), Add)) in
  exec_test ~expect:[ N 3 ] e;
  let e = T.Seq (Val (N 9), Seq (Val (B false), Cond (Val (N 3), Val (N 5)))) in
  exec_test ~expect:[ N 5; N 9 ] e;
  let e =
    T.Seq
      ( Val (N 9),
        Seq
          ( Val (B false),
            Seq (Cond (Val (N 3), Val (N 5)), Seq (Add, Seq (Val (N 14), Eql)))
          ) )
  in
  exec_test ~expect:[ B true ] e

let%test_unit "parse" =
  let result = Io.read "test.tsk" in
  let expect =
    T.Seq
      ( Val (N 9),
        Seq
          ( Val (B false),
            Seq (Cond (Val (N 3), Val (N 5)), Seq (Add, Seq (Val (N 14), Eql)))
          ) )
  in
  [%test_result: T.e] result ~expect
