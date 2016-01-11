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
module E = Errormsg
open Lockutil
open Cil
open Labelflow
module Lprof = Lockprofile

(*****************************************************************************)

let debug = ref false
let do_lockpick = ref false

(* subversion will substitute this *)
let version = "$Rev: 1533 $"

let print_version () =
  output_string stdout ("Lockpick build #"^version)

let compute_dominates_graph (shared: rhoSet) (atomic_list: rhoSet list)
    : rhoSet RhoHT.t =
  let dominates_hash = RhoHT.create (RhoSet.cardinal shared) in
  let does_not_dominate r r' =
    if !debug then ignore(E.log "%a does not dominate %a\n" d_rho r d_rho r');
    let tmp = RhoHT.find dominates_hash r in
    RhoHT.replace dominates_hash r (RhoSet.remove r' tmp)
  in
  RhoSet.iter
    (fun r ->
      ignore(E.log "init %a\n" d_rho r);
      RhoHT.replace dominates_hash r shared)
    shared;
  List.iter
    (fun atomic ->
      let notdom = RhoSet.diff shared atomic in
      RhoSet.iter
        (fun r -> RhoSet.iter (fun r' -> does_not_dominate r' r) notdom)
        atomic)
    atomic_list;
  dominates_hash

let dump_dominates_graph (g: rhoSet RhoHT.t) : unit =
  RhoHT.iter
    (fun r d ->
      RhoSet.iter
        (fun r' -> ignore(E.log "%a dominates %a\n" d_rho r d_rho r'))
        d
    ) g

let dump_solution (s: rho RhoHT.t) : unit =
  RhoHT.iter
    (fun r r' ->
      ignore(E.log "%a is protected by %a\n" d_rho r d_rho r');
    )
    s

let list_atomic_chi (shared: rhoSet) : rhoSet list =
  List.map
    (fun x -> let r,w = solve_chi_m x in
      let result = concrete_rhoset (RhoSet.inter (RhoSet.union r w) shared) in
      if !debug then ignore(E.log "chi: %a has effect: %a\n" d_chi x d_rhoset result);
      result
    )
    !all_atomic_chi

let options =
  [
    "--dolockpick",
      Arg.Set(do_lockpick),
      " Run the Lockpick algorithm for locking of atomic sections.";

    "--debug-lockpick",
      Arg.Set(debug),
      " Print lockpick profiling information after each phase.";
  ]

(* Joonghoon Lee 631 begin *)

(* first, find all atomics *)
let function_list = ref []
class findAtomicsVisitor : cilVisitor =
object (self)
    inherit nopCilVisitor
    method vinst (ins: instr) : instr list visitAction =
        (match ins with
        Call(_, Lval (Var (v:varinfo), NoOffset), args, loc) -> ()
        | _-> ());
        DoChildren
    method vfunc fd =
        function_list := fd.svar::!function_list;
        DoChildren
end
(* Joonghoon Lee 631 end *)


let doit (f: file) : unit = begin
  let shared = concrete_rhoset !Shared.all_shared_rho in
  let atomic_list = list_atomic_chi shared in
  let shared_atomic = List.fold_left
    (fun rs d -> RhoSet.union rs d) RhoSet.empty atomic_list
  in
  let dom = compute_dominates_graph shared_atomic atomic_list in
  if !debug then (
    ignore(E.log "dominates:\n");
    dump_dominates_graph dom;
  );

  (* algorithm 2 in the paper: *)
  let solution = RhoHT.create (RhoSet.cardinal shared_atomic) in
  RhoSet.iter (fun r -> RhoHT.replace solution r r) shared_atomic;
  RhoHT.iter
    (fun r dominated -> 
      RhoSet.iter
        (fun r' ->
          let s = RhoHT.find solution r in
          RhoHT.replace solution r' s;
          RhoHT.replace dom r' RhoSet.empty;
        )
        dominated
    )
    dom;
  if !debug then (
    ignore(E.log "dominates:\n");
    dump_solution solution;
  );

  let used = RhoHT.fold
    (fun _ r d -> RhoSet.add r d)
    solution
    RhoSet.empty
  in
  ignore(E.log "atomic sections    : %d\n" (List.length atomic_list));
  ignore(E.log "shared locations   : %d\n" (RhoSet.cardinal shared));
  ignore(E.log "shared atomic loc. : %d\n" (RhoSet.cardinal shared_atomic));
  ignore(E.log "used locks         : %d\n" (RhoSet.cardinal used));

(* Joonghoon Lee 631 begin *)
  (* find all locks that need to be acquired:
   create a fresh lock id for each rho in 'used'
   and a mapping from rho->lock *)

  (* Create a Hash Table mapping rho's to locks(ints) *)
  let rho_to_lock = RhoHT.create (RhoSet.cardinal shared_atomic) in
  (* and a counter for locks *)
  let lock_count = ref 0 in
  (*ignore(E.log "-----%s\n" (Int32.to_string (Int32.of_int !lock_count)));*)
  (* First, create a fresh lock name for each rho in 'used' *)
  RhoSet.iter
    (fun r ->
        lock_count := !lock_count + 1;
        RhoHT.replace rho_to_lock r !lock_count
     )
    used;
  (* Complete RH by making constructing for all rho->(rho->)lock *)
  RhoSet.iter
    (fun r ->
        let rl = RhoHT.find solution r in
        RhoHT.replace rho_to_lock r (RhoHT.find rho_to_lock rl))
    shared_atomic;
  (* print *)
  if !debug then
    (print_string "all locks:";
    RhoHT.iter (fun r l -> print_int l) rho_to_lock;
    print_string "\n");

   (*for all f : Functions
   1. get the chi of 'f'
   2. get all the rhos in that chi
   3. look up these rhos in the solution to
      find the corresponding set of rhos(that protect)
      these will map to there lock (created above)
   here: look up all the rhos in this function's chi, and get a list
   of locks to acquire.
   for each lock in locks: *)

    (let lpvisitor = new findAtomicsVisitor in
        visitCilFileSameGlobals lpvisitor f);

    if (RhoSet.cardinal shared_atomic)=0 then ()
    else
    ignore(E.log "\n");
    ignore(E.log "***************  Flux Constraint suggestions  **************\n");
    Strmap.iter
        (fun vname x -> let r,w = solve_chi_m x in
          let result = concrete_rhoset (RhoSet.inter (RhoSet.union r w) shared) in
          if !debug then ignore(E.log "chi:%a has effect:%a\n" d_chi x d_rhoset result);
          let cur_rho_list = ref [] in
          (RhoSet.iter
            (fun r ->
                (* put these into set and remove redundancy *)
                let lck = (RhoHT.find rho_to_lock r) in
                if (List.exists (fun n -> n=lck) !cur_rho_list)=false then
                    cur_rho_list := (RhoHT.find rho_to_lock r)::!cur_rho_list;
                result;())
            result);
          (* ignore(E.log "locks: "); *)
          let output_string = ref [] in

          if !cur_rho_list <> [] then
          (
              cur_rho_list := (List.fast_sort (fun a b -> a-b) !cur_rho_list);
              ignore(E.log "<%s> needs the following constraint(s).\n" vname);
              ignore(E.log " - %s : { " vname);
              ignore(E.log "%d " (List.hd (!cur_rho_list)));
              (List.iter
               (fun n ->
                    ignore(E.log ",%d " n))
                    (List.tl !cur_rho_list));
              ignore(E.log "}\n")
          );
        )
        !Locktype.fun_to_chi
(* Joonghoon Lee 631 end *)

end
