module Make_tests (Name : sig
  val name : string
end) (Uf : Union_find.DSU) =
struct
  module Spec = struct
    type t = Uf.t

    let universe_size = 6
    let init () = Uf.create universe_size
    let cleanup _ = ()

    open Lin

    let normalize x = x mod universe_size
    let union uf x y = Uf.union uf (normalize x) (normalize y)
    let same_set uf x y = Uf.same_set uf (normalize x) (normalize y)
    let find uf x = Uf.find uf (normalize x)

    let api =
      [
        val_ "union" union (t @-> nat_small @-> nat_small @-> returning bool);
        val_ "same_set" same_set
          (t @-> nat_small @-> nat_small @-> returning bool);
        val_ "find" find (t @-> nat_small @-> returning int);
      ]
  end

  module Domain_tests = Lin_domain.Make (Spec)

  let test =
    Domain_tests.lin_test ~count:200 ~name:(Name.name ^ " linearizability")
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
    [ Mutex_tests.test; Node_lock_tests.test; Cas_tests.test ]
