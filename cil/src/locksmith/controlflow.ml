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
module E = Errormsg
module LF = Labelflow
module Q = Queue
open Util

let debug = ref false

let options = [
  "--debug-controlflow",
     Arg.Set(debug),
     "Print control flow (phi graph) debugging output.";
]

exception ControlFlowBug of string

type phi = {
  phi_id : int;
  phi_loc : Cil.location;
  phi_name : string;
  mutable phi_global : bool; (* true if it's global *)

  (* edges *)
  mutable phi_next: phi list; (* successors *)
  mutable phi_prev: phi list; (* predecessors *)
  mutable phi_open_in: (LF.instantiation * phi) list; (* means p (i-> this *)
  mutable phi_open_out: (LF.instantiation * phi) list; (* means this (i -> p *)
  mutable phi_close_in: (LF.instantiation * phi) list; (* means p )i-> this *)
  mutable phi_close_out: (LF.instantiation * phi) list; (* means this )i -> p *)
}

module Phi : Set.OrderedType with type t = phi =
  struct
    type t = phi
    let compare (x: t) (y: t) : int =
      Pervasives.compare x.phi_id y.phi_id
  end
module PhiSet = Set.Make(Phi)

(* a hashtable from phi to 'a.
 * it is faster than (phi, 'a) Hashtbl.t,
 * because it uses phi_id to compare
 *)
module PhiHT =
  struct
    type t = phi
    let equal (x: t) (y: t) : bool =
      (Pervasives.compare x.phi_id y.phi_id = 0)
    let hash (x: t) : int = x.phi_id
  end
module PH = Hashtbl.Make(PhiHT)

type phiSet = PhiSet.t

(* all phi nodes in the control flow graph *)
let all_phi : phi list ref = ref []

(* "empty" program point.
 * Auxiliary phi used to type effect-less expressions
 *)
let empty_phi : phi = {
  phi_id = 0;
  phi_loc = Cil.locUnknown;
  phi_name = "empty";
  phi_global = false;
  phi_next = [];
  phi_prev = [];
  phi_open_in = [];
  phi_open_out = [];
  phi_close_in = [];
  phi_close_out = [];
}

(*********************
 * graph construction
 *********************)

(* a counter used to generate unique phi_ids *)
let phi_no : int ref = ref 0

(* creates a fresh phi node, corresponding to !currentLoc program point,
 * with kind k
 *)
let make_phi (s: string) : phi =
  incr phi_no;
  let p = {
    phi_id = !phi_no;
    phi_loc = !Cil.currentLoc;
    phi_name = s;
    phi_global = false;
    phi_next = [];
    phi_prev = [];
    phi_open_in = [];
    phi_open_out = [];
    phi_close_in = [];
    phi_close_out = [];
  } in
  all_phi := p::!all_phi;
  p

let starting_phis : phi list ref = ref []

(* creates a control flow edge from phi1 to phi2
 * fails if any of the two is the empty_phi
 *)
let phi_flows (phi1: phi) (phi2: phi) : unit = begin
  if phi1 == phi2 then ()
  else if Phi.compare phi1 empty_phi = 0 then ()
  else if Phi.compare phi2 empty_phi = 0
  then raise (ControlFlowBug "no phi can flow to the empty phi")
  else begin
    phi1.phi_next <- phi2::phi1.phi_next;
    phi2.phi_prev <- phi1::phi2.phi_prev;
  end
end

(* for every i, contains the renaming from abstract to concrete phis *)
(*let phi_renaming_map : (phi*phi) list LF.IH.t = LF.IH.create 10*)

(* create an instantiation edge (renaming) instantiating phi_abs to phi_inst
 * at instantiation i
 *)
let inst_phi (phi_abs: phi)
             (phi_inst: phi)
             (polarity: bool)
             (i: LF.instantiation) : unit =
(*  let maplist =
    try
      LF.IH.find phi_renaming_map i
    with
      Not_found -> []
  in
  LF.IH.replace phi_renaming_map i ((phi_abs,phi_inst)::maplist);
*)
  if polarity then begin
    phi_abs.phi_close_out <- (i,phi_inst)::phi_abs.phi_close_out;
    phi_inst.phi_close_in <- (i,phi_abs)::phi_inst.phi_close_in;
  end
  else begin
    phi_abs.phi_open_in <- (i,phi_inst)::phi_abs.phi_open_in;
    phi_inst.phi_open_out <- (i,phi_abs)::phi_inst.phi_open_out;
  end
  

(* Marks a phi point as global, unless it is in the set qs.
 * Global phis are the ones anottating global function pointers, for example.
 *)
let set_global_phi (p: phi) (qs: phiSet) : unit =
  if not (PhiSet.mem p qs) then
  p.phi_global <- true

(* unify two phi program points.  equivalent to each flowing to the other *)
let unify_phi (x: phi) (y: phi) : unit =
  if x == y then ()
  else begin
    phi_flows x y;
    phi_flows y x
  end

(* return a new phi to which both p1 and p2 flow *)
let join_phi (p1: phi) (p2: phi) : phi =
  if p1 == p2 then p1
  else begin
    let np = make_phi "" in
    phi_flows p1 np;
    phi_flows p2 np;
    np
  end

exception Found
(* returns true if control flow can reach p2 starting from p1 *)
(*
let reaches (p1: phi) (p2: phi) : bool =
  let visited : phiSet ref = ref PhiSet.empty in
  let rec dfs_visit_m (p: phi) : unit =
    if Phi.compare p p2 = 0 then raise Found
    else if PhiSet.mem p !visited then ()
    else begin
      visited := PhiSet.add p !visited;
      List.iter
        (fun p -> dfs_visit_m p)
        p.phi_next;
      List.iter
        (fun (i,p) -> dfs_visit_i i p)
        p.phi_inst_in;
    end
  and dfs_visit_i (i: LF.instantiation) (p: phi) : unit =
    if Phi.compare p p2 = 0 then raise Found
    else if PhiSet.mem p !visited then ()
    else begin
      visited := PhiSet.add p !visited;
      List.iter
        (fun p -> dfs_visit_m p)
        p.phi_next;
      List.iter
        (fun (i',p) -> if LF.InstHT.equal i i' then dfs_visit_m p)
        p.phi_inst_out;
    end
  in
    try
      dfs_visit_m p1; false
    with Found -> true
*)

(******************
 * pretty printing
 ******************)
(* phi2string *)
let dotstring_of_phi phi =
  "phi#" ^ (string_of_int phi.phi_id) ^ "\\n" ^
  (Pretty.sprint 80 (Cil.d_loc () phi.phi_loc))

let d_phi () (p: phi) : doc =
  text (p.phi_name^" at ") ++ Cil.d_loc () p.phi_loc

(* phiSet formatting *)
let d_phiset () (ps: phiSet) : doc =
  let f : phi -> doc -> doc = fun x d ->
    d ++ (if d <> nil then line else nil) ++ d_phi () x
  in
  align ++ (PhiSet.fold f ps nil) ++ unalign

(*****************
 * graph printing
 *****************)

(* start set is the set of nodes to print even if they have no edges *)
let print_graph (outf: out_channel) (f: phi -> bool) : unit = begin
  let ps = ref PhiSet.empty in
  let print_phi_edges (outf: out_channel) (p: phi) : unit = begin
    List.iter
      (fun p' -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\";\n"
          (dotstring_of_phi p) (dotstring_of_phi p'))
      p.phi_next;
    List.iter
      (fun (i,p') -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\" [label=\"(%s\"];\n"
          (dotstring_of_phi p) (dotstring_of_phi p')
          (LF.dotstring_of_inst i))
      p.phi_open_out;
    List.iter
      (fun (i,p') -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\" [label=\")%s\"];\n"
          (dotstring_of_phi p) (dotstring_of_phi p')
          (LF.dotstring_of_inst i))
      p.phi_close_out;
  end
  in
  List.iter
    (fun p ->
      print_phi_edges outf p;
      if f p then ps := PhiSet.add p !ps;
    ) !all_phi;
  PhiSet.iter
    (fun p ->
      Printf.fprintf outf "\"%s\" [shape=\"diamond\"];\n"
        (dotstring_of_phi p);
      if p.phi_global then
        Printf.fprintf outf "\"%s\" [peripheries=2];\n"
          (dotstring_of_phi p))
    !ps
end


module type ForwardsTransfer =
  sig
    type state
    val state_before_phi : state PH.t
    val transfer_fwd : phi -> phi Q.t -> state -> state option
    val starting_state : phi -> state option
    val merge_state : state -> state -> state
    val equal_state : state -> state -> bool
    val translate_state_in : state -> Labelflow.instantiation -> state
    val translate_state_out : state -> Labelflow.instantiation -> state
    val check_state : phi -> state -> unit
    val pretty : unit -> state -> doc
  end

module type ForwardsAnalysis =
  sig
    type state
    (* argument is initial worklist *)
    val solve : phi list -> unit
  end

module MakeForwardsAnalysis =
  functor (A: ForwardsTransfer) ->
  struct
    type state = A.state

    let worklist : phi Q.t = Q.create ()

    let get_or_create_state (p: phi) : state option =
      try
        Some (PH.find A.state_before_phi p)
      with Not_found -> A.starting_state p

    let set_or_merge_state (p: phi) (s: state) : unit = begin
      let s' = 
        try
        PH.find A.state_before_phi p
        with Not_found -> begin
          Q.add p worklist;
          s
        end
      in
      let s'' = A.merge_state s s' in
      if !debug then ignore(E.log "  old state before %a: %a\n"
                           d_phi p A.pretty s');
      if !debug then ignore(E.log "  incoming state before %a: %a\n"
                           d_phi p A.pretty s);
      if !debug then ignore(E.log "  new state before %a: %a\n"
                           d_phi p A.pretty s'');
      PH.replace A.state_before_phi p s'';
      if not (A.equal_state s' s'') then Q.add p worklist
    end

    let propagate_through (p: phi) : unit = begin
      if !debug then ignore(E.log "propagate through %a\n" d_phi p);
      let before_state = get_or_create_state p in
      if isSome before_state then begin
        let before_state = getSome before_state in
        if !debug then ignore(E.log "  state before: %a\n" A.pretty before_state);
        let after_state = A.transfer_fwd p worklist before_state in
        if isSome after_state then begin
          let after_state = getSome after_state in
          if !debug then ignore(E.log "  state after: %a\n" A.pretty after_state);
          if !debug then ignore(E.log "  d edges to:\n");
          List.iter
            (fun p' -> set_or_merge_state p' after_state)
            p.phi_next;
          if !debug then ignore(E.log "  open edges to:\n");
          List.iter
            (fun (i,p') ->
              let s = A.translate_state_in after_state i in
              set_or_merge_state p' s)
            p.phi_open_out;
          if !debug then ignore(E.log "  close edges to:\n");
          List.iter
            (fun (i,p') ->
              let s = A.translate_state_out after_state i in
              set_or_merge_state p' s)
            p.phi_close_out
        end
      end (* else Q.add p worklist*)
    end

    let solve start_list : unit = begin
      Q.clear worklist;
      if !debug then ignore(E.log "starting points:\n");
      List.iter
        (fun p ->
          if !debug then ignore(E.log "  %a\n" d_phi p);
          Q.add p worklist)
        start_list;
      let rec loop () : unit =
        propagate_through (Q.take worklist);
        loop ();
      in
      try
        loop ();
      with Q.Empty -> ();
    end
          
  end

module type BackwardsTransfer =
  sig
    type state
    val state_after_phi : state PH.t
    val transfer_back : phi -> phi Q.t -> state -> state
    val starting_state : phi -> state
    val merge_state : state -> state -> state
    val equal_state : state -> state -> bool
    val translate_state_in : state -> Labelflow.instantiation -> state
    val translate_state_out : state -> Labelflow.instantiation -> state
    val check_state : phi -> state -> unit
    val pretty : unit -> state -> doc
  end


module type BackwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeBackwardsAnalysis =
  functor (A: BackwardsTransfer) ->
  struct
    type state = A.state

    let worklist : phi Q.t = Q.create ()

    let get_or_create_state (p: phi) : state =
      try
        PH.find A.state_after_phi p
      with Not_found -> A.starting_state p

    let set_or_merge_state (p: phi) (s: state) : unit = begin
      (*ignore(E.log "merging before %a\n" d_phi p);*)
      let s' = 
        try
        PH.find A.state_after_phi p
        with Not_found ->
          Q.add p worklist;
          s
      in
      let s'' = A.merge_state s s' in
      if !debug then begin
        ignore(E.log "  old state after %a: %a\n" d_phi p A.pretty s');
        ignore(E.log "  input state after %a: %a\n" d_phi p A.pretty s);
        ignore(E.log "  new state after %a: %a\n" d_phi p A.pretty s'');
      end;
      PH.replace A.state_after_phi p s'';
      if not (A.equal_state s' s'') then begin
        Q.add p worklist
      end
    end

    let propagate_through (p: phi) : unit = begin
      if !debug then ignore(E.log "propagate through %a\n" d_phi p);
      let after_state = get_or_create_state p in
      let before_state = A.transfer_back p worklist after_state in
      if !debug then ignore(E.log "  input state: %a\n" A.pretty before_state);
          if !debug then ignore(E.log "sub\n");
      (* merge this with each predecessor's state *)
      List.iter (fun p' -> set_or_merge_state p' before_state) p.phi_prev;

          if !debug then ignore(E.log "in\n");
      (* translate along all open_i edges that end on p *) 
      List.iter
        (fun (i,p') ->
          let s = A.translate_state_out before_state i in
          set_or_merge_state p' s)
        p.phi_open_in;

          if !debug then ignore(E.log "out\n");
      (* translate along all close_i edges that end on p *) 
      List.iter
        (fun (i,p') ->
          let s = A.translate_state_in after_state i in
          set_or_merge_state p' s)
        p.phi_close_in;
    end

    let solve start_list : unit = begin
      Q.clear worklist;
      List.iter (fun p -> Q.add p worklist) start_list;
      let rec loop () : unit =
        propagate_through (Q.take worklist);
        loop ();
      in
      try
        loop ();
      with Q.Empty -> ();
    end
          
  end

