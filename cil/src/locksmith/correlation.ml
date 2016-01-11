(*
 *
 * Copyright (c) 2004-2006, 
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

module Q = Queue
module E = Errormsg


(*****************************************************************************)

let do_print_guarded_by = ref false
let debug = ref false
let do_group_warnings = ref false

type guard = { (* these are computed at solution time *)
  guard_id: int;
  guard_closed: bool; (* if true the acq set doesn't grow
                               * (it went through an exists instantiation) *)
  guard_rhos : rhoSet;
  guard_locks : lockSet;
  guard_location: Cil.location;
  mutable guard_contexts : instantiation list
}

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
]

exception CorrelationBug of string

module DerefH = Hashtbl.Make(
  struct
    (* what is dereferenced and where *)
    type t = rho * phi * effect * Cil.location

    let equal (r1,p1,e1,l1) (r2,p2,e2,l2) =
      (RhoHT.equal r1 r2)
        && (PhiHT.equal p1 p2)
        && (Effect.compare e1 e2 = 0)
        && (Cil.compareLoc l1 l2 = 0)

    let hash (r,p,_,_) =
      (* hopefully not all derefs will be at the same location
         and/or have the same effect *)
      2 * (RhoHT.hash r) + (PhiHT.hash p)
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

let id = ref 0
(* create a correlation edge *)
let make_guard (rs: rhoSet)
               (ls: lockSet)
               (frompack: bool)
               (l: Cil.location)
               (il: instantiation list)
               : guard = begin
  assert (not (RhoSet.is_empty rs));
  incr id;
  let g = {
    guard_id = !id;
    guard_closed = frompack;
    guard_rhos = close_rhoset_m rs;
    guard_locks = close_lockset ls;
    guard_location = l;
    guard_contexts = il;
  } in
  g
end


(***********
 * solution
 ***********)

let inst_guard_map_in : (guard * guard) IH.t = IH.create 1000
let inst_guard_map_out : (guard * guard) IH.t = IH.create 1000

let is_global_guard (g: guard) : bool =
  (RhoSet.exists is_global_rho g.guard_rhos) &&
  (LockSet.exists is_global_lock g.guard_locks || LockSet.is_empty g.guard_locks)

let clone_guard_out (i: instantiation) (g: guard) : guard option =
  if List.exists (fun i' -> InstHT.equal i i') g.guard_contexts then Some g
  else begin
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
      IH.add inst_guard_map_out i (g,gg);
      if !debug then ignore(E.log "make_guard returned\n");
      Some gg
    end
  end

let clone_guard_in (i: instantiation) (g: guard) : guard option =
  if ( List.exists (fun i' -> InstHT.equal i i') g.guard_contexts) then None
  else
  let r' = translate_rhoset_in i g.guard_rhos in
  let r = close_rhoset_m r' in
  let l = close_lockset (translate_lockset_in i g.guard_locks) in
  if (RhoSet.is_empty r) then begin
    if !debug then
      ignore(E.log "losing guard %a inwards through %a (rhoset becomes empty)\n"
        (d_guard nil) g d_instantiation i);
    None
  end
  else begin
    if !debug then begin
      ignore(E.log "call make_guard for cloning a guard (in) through %a\n" d_instantiation i);
      if (is_pack i) then ignore(E.log "(pack)\n)");
    end;
    let gg = make_guard r l (g.guard_closed || is_pack i)
                        g.guard_location g.guard_contexts in
    IH.add inst_guard_map_in i (gg,g);
    if !debug then ignore(E.log "make_guard returned\n");
    Some gg
  end

exception Compare_lists_found
let compare_lists l1 l2 eq =
  if ((List.length l1) <> (List.length l2)) then false
  else
  try
    List.iter2 (fun x1 x2 ->
      if (not (eq x1 x2))
      then raise Compare_lists_found)
      l1 l2;
      true
  with Compare_lists_found -> false

let guard_equals (g1: guard) (g2: guard) : bool =
  (*g1.guard_location = g2.guard_location &&*)
  g1.guard_closed = g2.guard_closed &&
  RhoSet.equal g1.guard_rhos g2.guard_rhos &&
  LockSet.equal g1.guard_locks g2.guard_locks (*&&
  compare_lists g1.guard_contexts g2.guard_contexts inst_equal*)

let translate_guard_out (i: instantiation) (g: guard) : guard option =
  try
    let guardmap = IH.find_all inst_guard_map_out i in
    let _,gg = List.find (fun (g1,_) -> guard_equals g g1) guardmap in
    Some gg
  with Not_found -> clone_guard_out i g
  
let translate_guard_in (i: instantiation) (g: guard) : guard option =
  try
    let guardmap = IH.find_all inst_guard_map_in i in
    let gg,_ = List.find (fun (_,g2) -> guard_equals g g2) guardmap in
    Some gg
  with Not_found -> clone_guard_in i g

let protect_map : lockSet RH.t = RH.create 100
let empty_state = GBSet.empty

module GBTransfer : BackwardsTransfer with type state = GBSet.t =
  struct
    type state = GBSet.t
    let state_after_phi = PH.create 1000
    let check_state _ _ = ()
    let starting_state _ = empty_state
    let equal_state = GBSet.equal
    let merge_state = GBSet.union
    let transfer_back p worklist gbset =
      let k = get_phi_kind p in
      match k with
        PhiPack p' ->
          let old =
            try PH.find state_after_phi p'
            with Not_found -> empty_state
          in
          PH.replace state_after_phi p' (merge_state old gbset);
          Q.add p' worklist;
          gbset
      | PhiSplitCall(_, p', l) -> 
          let (a) = get_split_state p' in
          if !debug then ignore(E.log "going through split at %a, adding %a\n" Cil.d_loc l d_lockset a);
          GBSet.fold
            (fun g acc ->
              GBSet.add 
                (if g.guard_closed then g
                  else { g with guard_locks = LockSet.union g.guard_locks a})
                acc)
            gbset
            GBSet.empty
      | PhiSplitReturn(_, p') ->
          let old =
            try PH.find state_after_phi p'
            with Not_found -> empty_state
          in
          PH.replace state_after_phi p' (merge_state old gbset);
          Q.add p' worklist;
          empty_state
      | _ -> gbset
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

let init_guards () : phi list = begin
  (* traverse derefs in order by index to canonicalize race reporting *)
  if !debug then ignore(E.log "initializing guards\n");
  PH.clear GBTransfer.state_after_phi;
  let list_derefs =
    DerefH.fold
      (fun (r,p,e,l) idx rest -> (r,p,e,idx,l)::rest)
      all_derefs
      []
  in
  let _ = DerefH.clear all_derefs in
  let sorted_derefs =
    List.sort
      (fun (r1,p1,e1,idx1,l1) (r2,p2,e2,idx2,l2) ->
        if idx1 = idx2 then 0
        else if idx1 < idx2 then -1
        else 1)
      list_derefs
  in
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
            try PH.find GBTransfer.state_after_phi p
            with Not_found -> empty_state
          in
          PH.replace GBTransfer.state_after_phi p (GBSet.add g gset)
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
  List.map (fun (r,p,e,idx,l) -> p) list_derefs
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
          let old = try RH.find protect_map r with Not_found -> ls in
          RH.replace protect_map r (LockSet.inter old ls)
        )
        rs
    )
    (try PH.find GBTransfer.state_after_phi p
    with Not_found -> GBSet.empty)
  in
  List.iter scanphi !starting_phis;
end

let solve () : unit = begin
  let start_list = init_guards () in
  GBAnalysis.solve start_list;
  fill_protection_map ();
end

let get_protection_set (r: rho) : lockSet =
  try RH.find protect_map r
  with Not_found -> LockSet.empty

let racefound : rhoSet ref = ref RhoSet.empty

let d_rho_guards () (r: rho) : doc =
  (*let allgbset = List.fold_left
    (fun gs p -> GBSet.union gs
      (try PH.find GBTransfer.state_after_phi p
      with Not_found -> GBSet.empty))
    GBSet.empty
    !starting_phis
  in*)
  let f d p =
    GBSet.fold
      (fun g d ->
        if RhoSet.mem r g.guard_rhos
                  then d ++ (if d <> nil then line ++ line else nil) ++
                       (d_guard (d_phi () p)) () g
                  else d)
      (try PH.find GBTransfer.state_after_phi p
      with Not_found -> GBSet.empty)
      d
  in
  align ++ (
    List.fold_left f nil !starting_phis
  ) ++ unalign

let check_race (r: rho) : unit =
  (*ignore(E.log "checking protection for %a\n" d_rho r);*)
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
