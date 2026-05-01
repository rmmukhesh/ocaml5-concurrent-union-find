open QCheck
open STM

let universe_size = 8

type cmd =
  | Union of int * int
  | Find of int
  | Same_set of int * int

let show_cmd = function
  | Union (x, y) -> Printf.sprintf "Union(%d,%d)" x y
  | Find x -> Printf.sprintf "Find(%d)" x
  | Same_set (x, y) -> Printf.sprintf "Same_set(%d,%d)" x y

let arb_node = Gen.int_range 0 (universe_size - 1)

let arb_cmd _state =
    let generator =
    Gen.oneof
      [
        Gen.map2 (fun x y -> Union (x, y)) arb_node arb_node;
        Gen.map (fun x -> Find x) arb_node;
        Gen.map2 (fun x y -> Same_set (x, y)) arb_node arb_node;
      ]
  in
  QCheck.make ~print:show_cmd generator

module Model = struct
  type state = (int * int) list

  let build state =
    let uf = Sequential_union_find.create universe_size in
    List.iter
      (fun (x, y) -> ignore (Sequential_union_find.union uf x y))
      (List.rev state);
    uf

  let union_result state x y =
    let uf = build state in
    not (Sequential_union_find.same_set uf x y)

  let root state x =
    let uf = build state in
    Sequential_union_find.find uf x

  let same state x y =
    let uf = build state in
    Sequential_union_find.same_set uf x y

  let next_state cmd state =
    match cmd with
    | Union (x, y) when union_result state x y -> (x, y) :: state
    | _ -> state
end

module Make_spec (Uf : Union_find.DSU) = struct
  type sut = Uf.t
  type state = Model.state
  type nonrec cmd = cmd

  let arb_cmd = arb_cmd
  let init_state = []
  let next_state = Model.next_state
  let precond _cmd _state = true

  let run cmd sut =
    match cmd with
    | Union (x, y) -> Res (bool, Uf.union sut x y)
    | Find x -> Res (int, Uf.find sut x)
    | Same_set (x, y) -> Res (bool, Uf.same_set sut x y)

  let init_sut () = Uf.create universe_size
  let cleanup _ = ()

  let postcond cmd state result =
    match cmd, result with
    | Union (x, y), Res ((Bool, _), actual) ->
      Bool.equal actual (Model.union_result state x y)
    | Find x, Res ((Int, _), actual) ->
      Int.equal actual (Model.root state x)
    | Same_set (x, y), Res ((Bool, _), actual) ->
      Bool.equal actual (Model.same state x y)
    | _ -> false

  let show_cmd = show_cmd
end

module Make_tests (Name : sig
  val name : string
end) (Uf : Union_find.DSU) =
struct
  module Spec = Make_spec (Uf)
  module Seq = STM_sequential.Make (Spec)
  module Dom = STM_domain.Make (Spec)

  let tests =
    let concurrent_arb =
      Dom.arb_triple 15 10 Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
    in
    [
      Seq.agree_test ~count:300 ~name:(Name.name ^ " sequential stm");
      QCheck.Test.make ~count:150 ~retries:10
        ~name:(Name.name ^ " concurrent stm")
        concurrent_arb
        Dom.agree_prop_par;
    ]
end

module Mutex_tests =
  Make_tests
    (struct
      let name = "mutex"
    end)
    (Mutex_union_find)

module Node_lock_tests =
  Make_tests
    (struct
      let name = "node-lock"
    end)
    (Node_lock_union_find)

module Cas_tests =
  Make_tests
    (struct
      let name = "cas"
    end)
    (Cas_union_find)

let () =
  QCheck_base_runner.run_tests_main
    (Mutex_tests.tests @ Node_lock_tests.tests @ Cas_tests.tests)
