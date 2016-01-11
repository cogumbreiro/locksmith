(* wrapper
 * 1. remember cardinality
 * 2. equals tests first for pointer equality
 *)
module E = Errormsg 

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val split: elt -> t -> t * bool * t
  end

module Make(Ord: OrderedType) =
  struct
    module Wrapped = Set.Make(Ord)

    type elt = Wrapped.elt
    type t = {
      data: Wrapped.t;
      size: int;
    }

    let makelset s = {
      data = s;
      size = Wrapped.cardinal s;
    }

    let empty = makelset Wrapped.empty
    let is_empty s = (s.size = 0)
    let mem x s = Wrapped.mem x s.data
    let add x s = makelset (Wrapped.add x s.data)
    let singleton x = makelset (Wrapped.singleton x)
    let remove x s = makelset (Wrapped.remove x s.data)
    let union s1 s2 = makelset (Wrapped.union s1.data s2.data)
    let inter s1 s2 = makelset (Wrapped.inter s1.data s2.data)
    let diff s1 s2 = makelset (Wrapped.diff s1.data s2.data)
    let compare s1 s2 = Wrapped.compare s1.data s2.data
    let equal s1 s2 =
      let tmp =
      if not (s1.size = s2.size) then begin
        (*ignore(E.log "saved-one\n");*)
        false
      end
      else Wrapped.equal s1.data s2.data
      in
      (*if tmp then ignore(E.log "equal\n") else ignore(E.log "not-equal\n");*)
      tmp
    let subset s1 s2 = Wrapped.subset s1.data s2.data
    let iter f s = Wrapped.iter f s.data
    let fold f s accu = Wrapped.fold f s.data accu
    let for_all p s = Wrapped.for_all p s.data
    let exists p s = Wrapped.exists p s.data
    let filter p s = makelset (Wrapped.filter p s.data)
    let partition p s =
      let (s1, s2) = Wrapped.partition p s.data in
      (makelset s1, makelset s2)
    let cardinal s = s.size
    let elements s = Wrapped.elements s.data
    let min_elt s = Wrapped.min_elt s.data
    let max_elt s = Wrapped.max_elt s.data
    let choose s = Wrapped.choose s.data
    let split x s =
      let (l, pres, r) = Wrapped.split x s.data in
      (makelset l, pres, makelset r)
  end
