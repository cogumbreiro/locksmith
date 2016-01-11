(*
 *
 * Copyright (c) 2004-2007, 
 *  Polyvios Pratikakis <polyvios@cs.umd.edu>
 *  Michael Hicks       <mwh@cs.umd.edu>
 *  Jeff Foster         <jfoster@cs.umd.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
open Pretty
open Labelflow
open Lockstate
open Controlflow

module BW = Controlflow.BackwardsWorklist
module E = Errormsg
module Lprof = Lockprofile


(*****************************************************************************)

let do_print_guarded_by = ref false
let debug = ref false
let do_group_warnings = ref false
let do_one_by_one = ref false
let do_sort_derefs = ref false
let do_count_phi_visits = ref false
let do_hashcons = ref true

type guard = { (* these are computed at solution time *)
  guard_id: int;
  guard_closed: bool; (* if true the acq set doesn't grow
                               * (it went through an exists instantiation) *)
  guard_rhos : rhoSet;  (* pointers *)
  guard_locks : lockSet; (* mutexes *)

  guard_rhos_size : int; (* memoize set size *)
  guard_locks_size : int; (* memoize set size *)

  guard_location: Cil.location;  (* program location *)
  guard_contexts : instantiation list  (* path of instantiations from dereference to main() or fork() *)
}

let guard_equals (g1: guard) (g2: guard) : bool =
  if g1 == g2 then true
  else if !do_hashcons then false
  else begin
    g1.guard_closed = g2.guard_closed
      && g1.guard_locks_size = g2.guard_locks_size
      && g1.guard_rhos_size = g2.guard_rhos_size
      && (g1.guard_locks == g2.guard_locks || LockSet.equal g1.guard_locks g2.guard_locks)
      && (g1.guard_rhos == g2.guard_rhos || RhoSet.equal g1.guard_rhos g2.guard_rhos)
  end

(* for reproducing "the bug" *)
let comp1 g1 g2 = g1.guard_id - g2.guard_id
let comp2 g1 g2 = g2.guard_id - g1.guard_id
let compfn = ref comp1

module GBSet = Set.Make(struct
    type t = guard
    let compare g1 g2 = !compfn g1 g2
  end)

let switch_gborder () = compfn := comp2

let options = [
  "--list-guardedby",
    Arg.Set(do_print_guarded_by),
    "print all guarded-by information.";

  "--debug-guardedby",
    Arg.Set(debug),
    "Print verbose output when computing the guarded-by solution.";

  "--group-warnings",
    Arg.Set(do_group_warnings),
    "Don't group warnings for conflated locations.";

  "--switch-gborder",
    Arg.Unit(switch_gborder),
    "Change the guarded-by ordering.";

  "--do-one-by-one",
    Arg.Set(do_one_by_one),
    "Propagate one guarded-by at a time.";

  "--do-sort-derefs",
    Arg.Set(do_sort_derefs),
    "Sort initial guarded-by worklist by postorder CFG traversal.";

  "--count-phi-visits",
     Arg.Set(do_count_phi_visits),
     "Count how many times the guarded-by analysis visits every phi.";

  "--no-hashcons",
     Arg.Clear(do_hashcons),
     "Don't use hashconsing to ID guarded-by structs.";
]

exception CorrelationBug of string

module DerefH = Hashtbl.Make(
  struct
    (* what is dereferenced and where *)
    type t = rho * phi * effect * Cil.location

    let equal (r1,p1,e1,l1) (r2,p2,e2,l2) =
      (Rho.equal r1 r2)
        && (Phi.equal p1 p2)
        && (Effect.compare e1 e2 = 0)
        && (Cil.compareLoc l1 l2 = 0)

    let hash (r,p,_,_) =
      (* hopefully not all derefs will be at the same location
         and/or have the same effect *)
      2 * (Rho.hash r) + (Phi.hash p)
  end
 )

(* maps rho, phi to (unique id of deref, program location) *)
(* hacky way around duplicate guard generation in locktype.ml *)
let all_derefs : int DerefH.t = DerefH.create 10

(* uniq deref id generator *)
let deref_no : int ref = ref 0

(* mark a dereference of r in p *)
let deref (r: rho) (p: phi) (e: effect) : unit = begin
  if p == empty_phi then () else
  if not (DerefH.mem all_derefs (r,p,e,!Cil.currentLoc)) then
    begin
      incr deref_no;
      DerefH.add all_derefs (r,p,e,!Cil.currentLoc) !deref_no;
    end
  else if !debug then
    ignore(E.log "omitting existing dereference: %a %a %a at %a\n"
      d_rho r d_phi p d_effect e Cil.d_loc !Cil.currentLoc)
end

(* a list of all \corr edges *)
(*let all_top_guards : GBSet.t ref = ref GBSet.empty*)

(******************
 * pretty printing
 ******************)

(* guard formatting *)
let d_guard (phi_name: doc)() (g: guard) =
  let r = align
    ++ text "dereference at " ++ (Cil.d_loc () g.guard_location)
    ++ line ++ text "  "
    ++ align
      ++ text "locations possibly dereferenced: " ++ line ++ text "  " ++ (d_rhoset () g.guard_rhos) ++ line
      ++ text "locks acquired at dereference: " ++ line ++ text "  " ++ (d_lockset () g.guard_locks) ++ line
      ++ text "in: " ++ phi_name
      ++ (List.fold_left
           (fun d i -> d ++ text " -> " ++ d_instantiation () i)
           nil
           g.guard_contexts)
    ++ unalign
  ++ unalign in
  r


(*
let d_guardset () gset : doc =
  align ++ (
    GBSet.fold
      (fun g d -> d ++ (if d <> nil then line ++ line else nil) ++ d_guard () g)
      gset
      nil
  ) ++ unalign
*)




(***************
 * construction
 ***************)

exception Compare_lists_found of int
let compare_lists (l1: 'a list) (l2: 'a list) (cmp: 'a -> 'a -> int) : int =
  let n1 = List.length l1 in
  let n2 = List.length l2 in
  if n1 < n2 then -1
  else if n2 < n1 then 1
  else try
    List.iter2
      (fun x1 x2 ->
        let c = cmp x1 x2 in
        if c = 0 then ()
        else raise (Compare_lists_found c))
      l1 l2;
      0
  with Compare_lists_found c -> c

module GBMap = Map.Make(
  struct
    type t = guard
    let compare (g1: guard) (g2: guard) : int =
      if g1 == g2 then 0
      else if g1.guard_closed <> g2.guard_closed then
        if g1.guard_closed then 1 else -1
      else let l = LockSet.compare g1.guard_locks g2.guard_locks in
      if l < 0 then -1 else if l > 0 then 1
      else let r = RhoSet.compare g1.guard_rhos g2.guard_rhos in
      if r < 0 then -1 else if r > 0 then 1
      else 0
        (*compare_lists g1.guard_contexts g2.guard_contexts (fun i1 i2 -> (Inst.hash i1) - (Inst.hash i2))*)
  end
 )

let rec combine_inst_lists l1 l2 : instantiation list =
  match l1, l2 with
    [], [] -> []
  | [], l
  | l, [] -> l
  | i1::tl1, i2::tl2 ->
      if Inst.equal i1 i2 then i1::(combine_inst_lists tl1 tl2)
      else l1 @ l2

let all_guards = ref GBMap.empty
let guard_no = ref 0
(* create a correlation edge *)
let make_guard (rs: rhoSet)
               (ls: lockSet)
               (frompack: bool)
               (l: Cil.location)
               (il: instantiation list)
               : guard = begin
  assert (not (RhoSet.is_empty rs));
  incr guard_no;
  let rr = close_rhoset_m rs in
  let ll = close_lockset ls in
  let g = {
    guard_id = !guard_no;
    guard_closed = frompack;
    guard_rhos = rr;
    guard_locks = ll;
    guard_rhos_size = RhoSet.cardinal rr;
    guard_locks_size = LockSet.cardinal ll;
    guard_location = l;
    guard_contexts = il;
  } in
  if !do_hashcons then begin
    try
      let g' = GBMap.find g !all_guards in
      if !debug then ignore(E.log "make_guard: saved one\n");
      decr guard_no;
      g'
    with Not_found ->
      all_guards := GBMap.add g g !all_guards; g
  end else g
end


(***********
 * solution
 ***********)

let inst_guard_map_in : (guard * guard) InstHT.t = InstHT.create 1000
let inst_guard_map_out : (guard * guard) InstHT.t = InstHT.create 1000

let is_global_guard (g: guard) : bool =
  (RhoSet.exists is_global_rho g.guard_rhos) &&
  (LockSet.exists is_global_lock g.guard_locks || LockSet.is_empty g.guard_locks)

let clone_guard_out (i: instantiation) (g: guard) : guard option =
  (*if List.exists (fun i' -> Inst.equal i i') g.guard_contexts then Some g else*)
  begin
    let r' = translate_rhoset_out i g.guard_rhos in
    let r = close_rhoset_m r' in
    let l = close_lockset (translate_lockset_out i g.guard_locks) in
    if (RhoSet.is_empty r) then begin
      if !debug then
        ignore(E.log "losing guard %a through %a (rhoset becomes empty)\n"
          (d_guard nil) g d_instantiation i);
      None
    end
    else begin
      if !debug then begin
        ignore(E.log "call make_guard for cloning guard %a\n on %a\n" (d_guard nil) g d_instantiation i);
        if (is_pack i) then ignore(E.log "(pack)\n)");
      end;
      let gg = make_guard r l (g.guard_closed || is_pack i)
                          g.guard_location (i::g.guard_contexts) in
      InstHT.add inst_guard_map_out i (g,gg);
      if !debug then ignore(E.log "make_guard returned\n");
      Some gg
    end
  end

let clone_guard_in (i: instantiation) (g: guard) : guard option =
  (*if ( List.exists (fun i' -> Inst.equal i i') g.guard_contexts) then None else*)
  let r' = translate_rhoset_in i g.guard_rhos in
  let r = close_rhoset_m r' in
  let l = close_lockset (translate_lockset_in i g.guard_locks) in
  if (RhoSet.is_empty r) then begin
    if !debug then
      ignore(E.log "losing guard %a inwards through %a (rhoset becomes empty)\n"
        (d_guard nil) g d_instantiation i);
    None
  end else begin
    if !debug then begin
      ignore(E.log "call make_guard for cloning a guard (in) through %a\n" d_instantiation i);
      if (is_pack i) then ignore(E.log "(pack)\n)");
    end;
    let gg = make_guard r l (g.guard_closed || is_pack i)
                        g.guard_location (g.guard_contexts) in
    InstHT.add inst_guard_map_in i (gg,g);
    if !debug then ignore(E.log "make_guard returned\n");
    Some gg
  end

let translate_guard_out (i: instantiation) (g: guard) : guard option =
  try
    let guardmap = InstHT.find_all inst_guard_map_out i in
    let _,gg = List.find (fun (g1,_) -> guard_equals g g1) guardmap in
    Some gg
  with Not_found -> clone_guard_out i g
  
let translate_guard_in (i: instantiation) (g: guard) : guard option =
  try
    let guardmap = InstHT.find_all inst_guard_map_in i in
    let gg,_ = List.find (fun (_,g2) -> guard_equals g g2) guardmap in
    Some gg
  with Not_found -> clone_guard_in i g

let protect_map : lockSet RhoHT.t = RhoHT.create 100
let empty_state = GBSet.empty

module GBTransfer : BackwardsTransfer with type state = GBSet.t =
  struct
    type state = GBSet.t
    let state_after_phi = PhiHT.create 1000
    let check_state _ _ = ()
    let starting_state _ = empty_state
    let equal_state s1 s2 = ((GBSet.cardinal s1) = (GBSet.cardinal s2))
    let merge_state = GBSet.union
    let is_relevant p =
      try
        ignore(get_phi_kind p);
        true
      with Not_found -> p == empty_phi

    let transfer_back p worklist gbset =
      if !debug then ignore(E.log "transfering %d / %d guards\n" (GBSet.cardinal gbset) !guard_no);
      if p == empty_phi then GBSet.empty else begin
        (*ignore(E.log "worklist size %d\n" (BW.length worklist));*)
        let k = get_phi_kind p in
        match k with
          (*PhiPack p' ->
            let old =
              try PhiHT.find state_after_phi p'
              with Not_found -> empty_state
            in
            let newstate = (merge_state old gbset) in
            if not (equal_state old newstate) then begin
              PhiHT.replace state_after_phi p' newstate;
              BW.push p' worklist;
            end;
            gbset*)
        | PhiSplitCall(_, p', l) -> 
            let (a) = get_split_state p' in
            if !debug then ignore(E.log "going through split at %a, adding %a\n" Cil.d_loc l d_lockset a);
            GBSet.fold
              (fun g acc ->
                GBSet.add 
                  (if g.guard_closed then g
                    else
                      let gl = LockSet.union g.guard_locks a in
                      { g with guard_locks = gl;
                               guard_locks_size = LockSet.cardinal gl; })
                  acc)
              gbset
              GBSet.empty
        | PhiSplitReturn(_, p') ->
            let old =
              try PhiHT.find state_after_phi p'
              with Not_found -> empty_state
            in
            if p' != empty_phi then begin
              let newstate = (merge_state old gbset) in
              if not (equal_state old newstate) then begin
                PhiHT.replace state_after_phi p' newstate;
                BW.push p' worklist;
              end;
            end;
            empty_state
        | _ -> gbset
      end
    let translate_state_out state inst =
      GBSet.fold
        (fun g s ->
          match translate_guard_out inst g with
            None -> s
          | Some gg -> GBSet.add gg s)
        state
        GBSet.empty
    let translate_state_in state inst =
      (*empty_state*)
      GBSet.fold
        (fun g s ->
          match translate_guard_in inst g with
            None -> s
          | Some gg -> GBSet.add gg s)
        state
        GBSet.empty
    let pretty () gbset =
      (GBSet.fold
        (fun g d -> d ++ line ++ (d_guard nil) () g)
        gbset
        align) ++ unalign
  end

module GBAnalysis = MakeBackwardsAnalysis(GBTransfer)

(* Applies f on every phi in the graph, visiting all nodes reachable from
 * starting_phis in post-order traversal (starting_phis are visited last)
 *)
(*let postorder_visit (f: phi -> unit) : unit =
  let visited : phiSet ref = ref PhiSet.empty in
  let rec visit (p: phi) : unit =
    if PhiSet.mem p !visited then ()
    else begin
      visited := PhiSet.add p !visited;
      let k =
        try get_phi_kind p 
        with Not_found -> PhiVar
      in
      let _ = match k with
        PhiSplitCall(_,phi_ret,_) ->
          visit phi_ret
        | _ -> ()
      in
      List.iter
        (fun p -> visit p)
        (get_all_children p);
      f p
    end
  in
  List.iter visit !starting_phis
*)

let init_guards () : phi list = begin
  if !debug then ignore(E.log "initializing guards\n");
  (* reset fs-state *)
  PhiHT.clear GBTransfer.state_after_phi;
  (* traverse derefs in order by index to canonicalize race reporting *)
  let list_derefs =
    DerefH.fold
      (fun (r,p,e,l) idx rest -> (r,p,e,idx,l)::rest)
      all_derefs
      []
  in
  (* throw away all_derefs, we don't need it any more *)
  let _ = DerefH.clear all_derefs in
  (* sort the list by index *)
  let sorted_derefs =
    List.sort
      (fun (r1,p1,e1,idx1,l1) (r2,p2,e2,idx2,l2) ->
        if idx1 = idx2 then 0
        else if idx1 < idx2 then -1
        else 1)
      list_derefs
  in
  (* for every deref, if it's shared, add a starting guard at that phi *)
  let initial_set = ref PhiSet.empty in
  List.iter
    (fun (r,p,e,idx,l) ->
      try
        if Shared.is_shared r p e then (
          if !debug then
            ignore(E.log "adding a starting guard for %a in %a\n"
              d_rho r d_phi p);
          let a = get_state_before p in (* acquired locks at p *)
          let g = make_guard (get_rho_p2set_m r) a false l [] in
          let gset = 
            try PhiHT.find GBTransfer.state_after_phi p
            with Not_found -> empty_state
          in
          PhiHT.replace GBTransfer.state_after_phi p (GBSet.add g gset);
          if !do_one_by_one then GBAnalysis.solve [p]
          else initial_set := PhiSet.add p !initial_set;
        ) else (
          if !debug then
            ignore(E.log "not counting dereference at %a\n" Cil.d_loc l);
        )
      with Not_found -> if !debug then
        ignore(E.log "ignoring dereference at non-reachable location %a\n"
                 Cil.d_loc l)
        (* if p doesn't have a state then it's in dead code, and
         * get_state_before raises Not_found.  I guess it's safe to ignore
         * that guard then.
         *)
    )
    sorted_derefs;
  (* the initial worklist is the list of all phi that have a guard *)
  (*List.map (fun (r,p,e,idx,l) -> p) list_derefs*)
  let sorted_list = ref [] in
  if !do_sort_derefs then (
    postorder_visit
      (fun p ->
        if PhiSet.mem p !initial_set
        then (
          initial_set := PhiSet.remove p !initial_set;
          sorted_list := p::!sorted_list;
        )
      );
    (*PhiSet.elements !initial_set*)
    !sorted_list
  ) else PhiSet.elements !initial_set
end

let fill_protection_map () : unit = begin
  let scanphi p = GBSet.iter
    (fun g ->
      let ls = g.guard_locks in
      let rs = concrete_rhoset g.guard_rhos in
      (*
      ignore(E.log "%a:\n %a correlates with %a \n\n"
                   d_guard g d_rhoset rs d_lockset ls);
      *)
      RhoSet.iter
        (fun r ->
          let old = try RhoHT.find protect_map r with Not_found -> ls in
          RhoHT.replace protect_map r (LockSet.inter old ls)
        )
        rs
    )
    (try PhiHT.find GBTransfer.state_after_phi p
    with Not_found -> GBSet.empty)
  in
  List.iter scanphi !starting_phis;
end

let solve () : unit = begin
  let start_list = init_guards () in
  Lprof.endtime "guarded-by:init-guards";
  if !debug then ignore(E.log "phase-begin(solve-guarded-by)\n");
  GBAnalysis.solve start_list;
  if !debug then ignore(E.log "phase-end(solve-guarded-by)\n");
  Lprof.endtime "guarded-by:propagation";
  (*List.iter (fun p -> ignore(E.log "phi: %a\n" d_phi p)) !all_phi;*)
  if !do_count_phi_visits
  then count_phi_visits
    (fun p -> string_of_int (GBSet.cardinal
      (try PhiHT.find GBTransfer.state_after_phi p
      with Not_found -> GBSet.empty))
    );
  fill_protection_map ();
end

let get_protection_set (r: rho) : lockSet =
  try RhoHT.find protect_map r
  with Not_found -> LockSet.empty

let racefound : rhoSet ref = ref RhoSet.empty

let d_rho_guards () (r: rho) : doc =
  (*let allgbset = List.fold_left
    (fun gs p -> GBSet.union gs
      (try PhiHT.find GBTransfer.state_after_phi p
      with Not_found -> GBSet.empty))
    GBSet.empty
    !starting_phis
  in*)
  let f d p =
    let gset =
      try PhiHT.find GBTransfer.state_after_phi p
      with Not_found -> GBSet.empty
    in
    let sorted_guards =
      List.sort
        (fun g1 g2 -> 
          if g1.guard_id = g2.guard_id then 0
          else let r = (Cil.compareLoc g1.guard_location g2.guard_location) in
          if r = 0 then !compfn g1 g2 else r)
        (GBSet.elements gset)
    in
    List.fold_left
      (fun d g ->
        if RhoSet.mem r g.guard_rhos
                  then d ++ (if d <> nil then line ++ line else nil) ++
                       (d_guard (d_phi () p)) () g
                  else d)
      d
      sorted_guards
  in
  align ++ (
    List.fold_left f nil !starting_phis
  ) ++ unalign

let check_race (r: rho) : unit =
  if !debug then ignore(E.log "checking protection for %a\n" d_rho r);
  if !do_group_warnings && RhoSet.mem r !racefound then () else
  if Shared.is_ever_shared r then begin
    (*ignore(E.log " It is shared, check protection set\n");*)
    let crs = concrete_rhoset (get_rho_p2set_m r) in
    let ls = get_protection_set r in
    if (LockSet.is_empty ls) then begin
      if !do_group_warnings then begin
        ignore(E.warn "Possible data race:\n unprotected locations:\n  %a\n references:\n  %a\n"
          d_rhoset crs d_rho_guards r);
      end else begin
        ignore(E.warn "Possible data race: %a is not protected!\n references:\n  %a\n"
          d_rho r d_rho_guards r);
      end;
      racefound := RhoSet.union crs !racefound;
    end else if (LockSet.is_empty (concrete_lockset ls)) then begin
      if !do_group_warnings then begin
        ignore(E.warn "Possible data race:\n locations:\n  %a protected by non-linear or concrete lock(s):\n  %a\n references:\n  %a\n"
          d_rhoset crs d_lockset ls d_rho_guards r);
      end else begin
        ignore(E.warn "Possible data race: %a is protected by non-linear or concrete lock(s):\n  %a\n references:\n  %a\n"
          d_rho r d_lockset ls d_rho_guards r);
      end;
      racefound := RhoSet.union crs !racefound;
    end else
      if !do_print_guarded_by then
        ignore(E.log "%a is protected by:\n  %a\n" d_rho r d_lockset ls);
  end
  (*else ignore(E.log " It's not shared, no need to protect it\n")*)

let check_races () : unit =
  Labelflow.concrete_rho_iter check_race

let escapes (l: lock) (ls, rs: lockSet * rhoSet) : bool =
  LockSet.mem l (close_lockset ls)
