type stats =
  {
    find_calls : int;
    union_calls : int;
    cas_attempts : int;
    cas_failures : int;
    path_compactions : int;
    rank_updates : int;
  }

module type DSU = sig
  type t

  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> bool
  val same_set : t -> int -> int -> bool
  val component_count : t -> int
  val stats : t -> stats
  val reset_stats : t -> unit
end
