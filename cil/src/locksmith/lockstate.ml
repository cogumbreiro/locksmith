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
module LF = Labelflow
module E = Errormsg
module CF = Controlflow
module Q = Queue

module LockSet = LF.LockSet
type lockSet = LF.lockSet
module PH = CF.PH
type phi = CF.phi

let debug = ref false

let options = [
  "--debug-lockstate",
    Arg.Set(debug),
    "trace info during lock-state computation.";
]

type phi_kind =
  | PhiVar
  | PhiForked
  | PhiPack of phi (* only propagate the guarded by from this node to phi *)
  | PhiNewlock of LF.lock
  | PhiAcquire of LF.lock
  | PhiRelease of LF.lock
  | PhiDelete of LF.lock
  | PhiSplitCall of LF.lock_effect * phi * Cil.location
  | PhiSplitReturn of LF.lock_effect * phi

(*type t = lockSet * lockSet*)
type t = lockSet

(* this hashtable is used to store the "split" state for every phi
 *)
let saved_split_state : t CF.PH.t = CF.PH.create 100

(*let empty_state = (LockSet.empty, LockSet.empty)*)
let empty_state = LockSet.empty

let get_split_state (p: phi) : t =
  try
    CF.PH.find saved_split_state p
  with Not_found -> empty_state

let set_split_state (p: phi) (newa: t) : unit =
  (*
  let olda,oldr = get_split_state p in
  PH.replace saved_split_state p (newa,newr)
  *)
  let olda = get_split_state p in
  PH.replace saved_split_state p newa
    
let phi_kind_hash : phi_kind PH.t = PH.create 100

let get_phi_kind phi = PH.find phi_kind_hash phi

(* looks up the kind of p in \tenv *)
let set_phi_kind (p: phi) (k: phi_kind) : unit = begin
  assert(not (PH.mem phi_kind_hash p));
  PH.replace phi_kind_hash p k;
end

let print_phi_kind (outf: out_channel) phi : bool =
  let k = PH.find phi_kind_hash phi in
  match k with
    PhiVar -> false
  | PhiForked -> false
  | PhiPack p ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"pack\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi) (CF.dotstring_of_phi p);
      true
  | PhiNewlock l ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"new\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi) (LF.dotstring_of_lock l);
      true
  | PhiAcquire l ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"acq\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi) (LF.dotstring_of_lock l);
      true
  | PhiRelease l ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"rel\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi) (LF.dotstring_of_lock l);
      true
  | PhiDelete l ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"del\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi) (LF.dotstring_of_lock l);
      true
  | PhiSplitCall (e,p, _) ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"call(%s)\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi)
        (CF.dotstring_of_phi p)
        (LF.dotstring_of_lock_effect e);
      true
  | PhiSplitReturn (e,p) ->
      Printf.fprintf
        outf "\"%s\" -> \"%s\" [label=\"return(%s)\", style=\"dotted\"];\n"
        (CF.dotstring_of_phi phi)
        (CF.dotstring_of_phi p)
        (LF.dotstring_of_lock_effect e);
      true

let print_graph outf = CF.print_graph outf (print_phi_kind outf)

module LockStateTransfer =
  struct
    type state = t

    let state_before_phi = PH.create 1000

    (* phi transfer function *)
    let transfer_fwd (p: phi) worklist (acq: state) : state option =
    begin
      let k = PH.find phi_kind_hash p in
      match k with
      | PhiVar -> Some acq
      | PhiForked -> Some LockSet.empty
      | PhiPack _ -> assert false (* nothing flows here *)
      | PhiNewlock l ->
          let ls = LF.get_lock_p2set l in
          Some acq
      | PhiAcquire l ->
          let ls = LF.get_lock_p2set l in
          Some (LockSet.union acq ls)
      | PhiRelease l ->
          let ls = LF.get_lock_p2set l in
          Some (LockSet.diff acq ls)
      | PhiDelete l ->
          let ls = LF.get_lock_p2set l in
          if not (LockSet.is_empty (LockSet.inter ls acq)) then
            ignore(E.log "deleting acquired lock %a\n" LF.d_lock l);
          Some (LockSet.diff acq ls)
      | PhiSplitCall (e,p',_) ->
          let acqin, acqout = LF.split_lockset acq e in
          set_split_state p' acqout;
          Q.add p' worklist;
          if !debug then
            ignore(E.log "split acq at %a\n cur:%a\n in: %a\n out: %a\n"
                  CF.d_phi p
                  LF.d_lockset acq
                  LF.d_lockset acqin
                  LF.d_lockset acqout);
          Some acqin
      | PhiSplitReturn (e,p') ->
          try
            let a = CF.PH.find saved_split_state p in
            Some (LockSet.union acq a)
          with Not_found -> None
    end

    let starting_state (p: phi) : state option = None

    let merge_state (acq1: state) (acq2: state) : state =
      let acq = LockSet.inter acq1 acq2 in
      (*
      ignore(E.log "merge state: %a\n" LF.d_lockset acq1);
      ignore(E.log "  and state: %a\n" LF.d_lockset acq2);
      ignore(E.log " into state: %a\n" LF.d_lockset acq);
      *)
      (LF.close_lockset acq)

    let equal_state (acq1: state) (acq2: state) : bool =
      (LockSet.equal acq1 acq2)

    let translate_state_in (acq: state) (i: LF.instantiation) : state =
      LF.translate_lockset_in i acq

    let translate_state_out (acq: state) (i: LF.instantiation) : state =
      LF.translate_lockset_out i acq

    let check_state (p: phi) (acq: state) : unit = ()

    let pretty () acq =
      align ++ text "acq: " ++ LF.d_lockset () acq
      (*++ text "rel: " ++ LF.d_lockset () rel*) ++ unalign
  end

module LS = CF.MakeForwardsAnalysis(LockStateTransfer)

let get_state_before phi = PH.find LockStateTransfer.state_before_phi phi

let solve () =
  List.iter
    (fun p -> PH.replace LockStateTransfer.state_before_phi p empty_state)
    !CF.starting_phis;
  LS.solve !CF.starting_phis
