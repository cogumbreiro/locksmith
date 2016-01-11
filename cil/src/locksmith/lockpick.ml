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
module E = Errormsg
open Lockdefs
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
    : rhoSet RH.t =
  let dominates_hash = RH.create (RhoSet.cardinal shared) in
  let does_not_dominate r r' =
    if !debug then ignore(E.log "%a does not dominate %a\n" d_rho r d_rho r');
    let tmp = RH.find dominates_hash r in
    RH.replace dominates_hash r (RhoSet.remove r' tmp)
  in
  RhoSet.iter
    (fun r ->
      ignore(E.log "init %a\n" d_rho r);
      RH.replace dominates_hash r shared)
    shared;
  List.iter
    (fun atomic ->
      let notdom = RhoSet.diff shared atomic in
      RhoSet.iter
        (fun r -> RhoSet.iter (fun r' -> does_not_dominate r' r) notdom)
        atomic)
    atomic_list;
  dominates_hash

let dump_dominates_graph (g: rhoSet RH.t) : unit =
  RH.iter
    (fun r d ->
      RhoSet.iter
        (fun r' -> ignore(E.log "%a dominates %a\n" d_rho r d_rho r'))
        d
    ) g

let dump_solution (s: rho RH.t) : unit =
  RH.iter
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
       "Run the Lockpick algorithm for locking of atomic sections.";

    "--debug-lockpick",
       Arg.Set(debug),
       "Print lockpick profiling information after each phase.";
  ]

let doit () : unit = begin
  let shared = concrete_rhoset !Shared.all_shared_rho in
  let atomic_list = list_atomic_chi shared in
  let shared_atomic = List.fold_left
    (fun rs d -> RhoSet.union rs d) RhoSet.empty atomic_list
  in
  let dom = compute_dominates_graph shared_atomic atomic_list in
  if !debug then dump_dominates_graph dom;

  (* algorithm 2 in the paper: *)
  let solution = RH.create (RhoSet.cardinal shared_atomic) in
  RhoSet.iter (fun r -> RH.replace solution r r) shared_atomic;
  RH.iter
    (fun r dominated -> 
      RhoSet.iter
        (fun r' ->
          let s = RH.find solution r in
          RH.replace solution r' s;
          RH.replace dom r' RhoSet.empty;
        )
        dominated
    )
    dom;
  if !debug then dump_solution solution;

  let used = RH.fold
    (fun _ r d -> RhoSet.add r d)
    solution
    RhoSet.empty
  in
  ignore(E.log "atomic sections    : %d\n" (List.length atomic_list));
  ignore(E.log "shared locations   : %d\n" (RhoSet.cardinal shared));
  ignore(E.log "shared atomic loc. : %d\n" (RhoSet.cardinal shared_atomic));
  ignore(E.log "used locks         : %d\n" (RhoSet.cardinal used));
end
