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
exception ControlFlowBug of string

val options : (string * Arg.spec * string) list
val debug : bool ref

(* program point *)
type phi

(* a hashtable from phi to 'a.
 * it is faster than (phi, 'a) Hashtbl.t,
 * because it uses phi_id to compare
 *)
module PH : Hashtbl.S with type key = phi
module PhiHT : Hashtbl.HashedType with type t = phi
module PhiSet : Set.S with type elt = phi
type phiSet = PhiSet.t

(* all phi nodes in the control flow graph *)
val all_phi : phi list ref

(* "empty" program point.
 * Auxiliary phi used to type effect-less expressions
 *)
val empty_phi : phi

(*********************
 * graph construction
 *********************)

(* creates a fresh phi node, corresponding to !currentLoc program point,
 *)
val make_phi : string -> phi

val starting_phis : phi list ref
(*val phi_calls : phi -> phi -> phi -> Labelflow.effect -> unit*)

(* creates a control flow edge from phi1 to phi2
 * fails if any of the two is the empty_phi
 *)
val phi_flows : phi -> phi -> unit

(* create an instantiation edge "control enters context" from the first
 * to the second argument at the given instantiation site.
 * polarity (not used (?) ) is true for positive.
 *)
val inst_phi : phi -> phi -> bool -> Labelflow.instantiation -> unit

(* Marks a phi point as global, unless it is in the set qs.
 * Global phis are the ones anottating global function pointers,
 * for example.
 *)
val set_global_phi : phi -> phiSet -> unit

(* unify two phi program points.
 * equivalent to each flowing to the other
 *)
val unify_phi : phi -> phi -> unit

(* return a new phi to which both p1 and p2 flow *)
val join_phi : phi -> phi -> phi

(* reachability. if true, then the first program point is followed by
 * the second in control flow
 *)

(******************
 * pretty printing
 ******************)
(* phi2string *)
val dotstring_of_phi : phi -> string
val d_phi : unit -> phi -> Pretty.doc

(* phiSet formatting *)
val d_phiset : unit -> phiSet -> Pretty.doc

val print_graph : out_channel -> (phi -> bool) -> unit

module type ForwardsTransfer =
  sig
    type state
    val state_before_phi : state PH.t
    val transfer_fwd : phi -> phi Queue.t -> state -> state option
    val starting_state : phi -> state option
    val merge_state: state -> state -> state
    val equal_state: state -> state -> bool
    val translate_state_in: state -> Labelflow.instantiation -> state
    val translate_state_out: state -> Labelflow.instantiation -> state
    val check_state: phi -> state -> unit
    val pretty : unit -> state -> Pretty.doc
  end

module type ForwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeForwardsAnalysis:
  functor (A: ForwardsTransfer) -> ForwardsAnalysis with type state = A.state

module type BackwardsTransfer =
  sig
    type state
    val state_after_phi : state PH.t
    val transfer_back : phi -> phi Queue.t -> state -> state
    val starting_state : phi -> state
    val merge_state: state -> state -> state
    val equal_state: state -> state -> bool
    val translate_state_in: state -> Labelflow.instantiation -> state
    val translate_state_out: state -> Labelflow.instantiation -> state
    val check_state: phi -> state -> unit
    val pretty : unit -> state -> Pretty.doc
  end

module type BackwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeBackwardsAnalysis:
  functor (A: BackwardsTransfer) ->
    BackwardsAnalysis with type state = A.state
