open Union_find

type counters =
  {
    find_calls : int Atomic.t;
    union_calls : int Atomic.t;
    cas_attempts : int Atomic.t;
    cas_failures : int Atomic.t;
    path_compactions : int Atomic.t;
    rank_updates : int Atomic.t;
  }

let create () =
  {
    find_calls = Atomic.make 0;
    union_calls = Atomic.make 0;
    cas_attempts = Atomic.make 0;
    cas_failures = Atomic.make 0;
    path_compactions = Atomic.make 0;
    rank_updates = Atomic.make 0;
  }

let rec add counter delta =
  let current = Atomic.get counter in
  if not (Atomic.compare_and_set counter current (current + delta)) then add counter delta

let bump counter = add counter 1

let snapshot counters : stats =
  {
    find_calls = Atomic.get counters.find_calls;
    union_calls = Atomic.get counters.union_calls;
    cas_attempts = Atomic.get counters.cas_attempts;
    cas_failures = Atomic.get counters.cas_failures;
    path_compactions = Atomic.get counters.path_compactions;
    rank_updates = Atomic.get counters.rank_updates;
  }

let reset counters =
  Atomic.set counters.find_calls 0;
  Atomic.set counters.union_calls 0;
  Atomic.set counters.cas_attempts 0;
  Atomic.set counters.cas_failures 0;
  Atomic.set counters.path_compactions 0;
  Atomic.set counters.rank_updates 0
