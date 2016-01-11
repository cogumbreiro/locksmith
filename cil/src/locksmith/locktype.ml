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
module U = Uref
module Q = Queue
module Lprof = Lockprofile
module Uniq = Uniqueness
module LV = Livevars
module LN = Labelname
module CG = Callgraph
module LF = Labelflow

open Cil
open Printf
open Pretty
open Lockdefs
open Labelflow
open Correlation
open Shared
open Lockstate
open Util
open Locksettings
module S = Semiunification

module CF = Controlflow
open Controlflow
(*
type phi = CF.phi
module PhiSet = CF.PhiSet
*)
type phiSet = PhiSet.t

let string_of_doc d = Pretty.sprint 800 d

let rec string_of_lock_exp e =
  match e with
    CastE(_,e) -> string_of_lock_exp e
  | AddrOf lv -> string_of_doc (d_exp () (Lval lv))
  | _ -> "*"^(string_of_doc (d_exp () e))

let string_of_exp e = string_of_doc (d_exp () e)

(* wrappers *)
let join_phi (p1: phi) (p2: phi) (k: phi_kind) : phi =
  if p1 == p2 then p1 else begin
    let p = join_phi p1 p2 in
    set_phi_kind p k;
    p
  end

let make_phi (s: string) (k: phi_kind) : phi =
  let p = make_phi s in
  set_phi_kind p k;
  p

(* user interface *)
let debug = ref false
let debug_void = ref false
let debug_one_effect = ref false

let do_void_conflate = ref false
let do_void_single = ref false
let do_uniq = ref true
let do_existentials = ref true
let do_down = ref true
let do_ignore_casts = ref true
let do_compact_structs = ref false
let do_one_effect = ref true
let do_context_insensitive = ref false

let options = [
  "--debug-typing",
     Arg.Set(debug),
     "Print progress information during the typechecking phase (constraint generation)";

  "--no-void",
     Arg.Set(do_void_conflate),
     "Conflate at void* casts";

  "--single-void",
     Arg.Set(do_void_single),
     "Conflate at void* casts for more than 1 types";

  "--debug-void",
     Arg.Set(debug_void),
     "Print debugging status information about void* and struct-field computation";

  "--no-uniqueness",
     Arg.Clear(do_uniq),
     "Don't use uniqueness analysis";

  "--no-existentials",
     Arg.Unit(fun () -> do_existentials := false; do_compact_structs := true),
     "Don't use existential types";

  "--no-down",
     Arg.Clear(do_down),
     "Don't apply [down] (loop bodies for alloc effects, and forks for cont. effects)";

  "--no-ignore-casts",
     Arg.Clear(do_ignore_casts),
     "Create subtyping constraints for each cast, rather than ignoring them";
  
  "--no-use-one-effect",
     Arg.Clear(do_one_effect),
     "Don't use only one effect variable for the whole body of non-forking functions";

  "--debug-one-effect",
     Arg.Set(debug_one_effect),
     "Use only one effect variable for the whole body of non-forking functions";

  "--context-insensitive",
     Arg.Set(do_context_insensitive),
     "Use normal flow for rhos instead of instantiation";
(*
  "--compact-structs",
     Arg.Set(do_compact_structs),
     "Allow struct pointers in structs to be unfolded once";
*)
]

exception TypingBug of string

let current_function : fundec ref = ref Cil.dummyFunDec
let current_chi: chi ref = ref (make_chi "X")
let pragmaKeyword : string = "existential"

(*****************************************************************************)
(* THIS IS VERY UGLY AND SHOULD GO AWAY:
 * Override to do nothing when lockpick isn't enabled.
 * It saves some chi labels from getting into banshee when not necessary... *)
let make_chi, chi_flows, set_atomic_chi, add_to_read_chi, add_to_write_chi,
    set_global_chi, inst_chi =
  if !Lockpick.do_lockpick then
    make_chi, chi_flows, set_atomic_chi, add_to_read_chi, add_to_write_chi,
    set_global_chi, inst_chi
  else
    let f1 _ = () in
    let f2 _ _ = () in
    let f4 _ _ _ _ = () in
    (fun _ -> !current_chi), f2, f1, f2, f2, f1, f4

let forced_effect: effect option ref = ref None
let force_effect (e: effect) : unit = begin
  assert (!forced_effect = None);
  forced_effect := Some e;
end
let unforce_effect () : unit = begin
  assert (!forced_effect <> None);
  forced_effect := None;
end
let make_effect x t =
  if Util.isSome !forced_effect
  then Util.getSome !forced_effect
  else LF.make_effect x t

let d_instantiation () i =
  LF.d_instantiation () i ++
    (if !debug then dprintf "(%s)" (dotstring_of_inst i) else nil)
(*****************************************************************************)

let functions_that_call_fork : (string, unit) Hashtbl.t = Hashtbl.create 117
let typenames : (string, Cil.typ) Hashtbl.t = Hashtbl.create 117
let locktypes : Cil.typ list ref = ref []
let locktypesigs : Cil.typsig list ref = ref []

class unrollTypeVisitor = object
  inherit nopCilVisitor
  method vglob = function
    | GType(ti, _) ->
        Hashtbl.add typenames ti.tname (Cil.unrollTypeDeep ti.ttype);
        DoChildren
    | GCompTag(ci,_) ->
        Hashtbl.add typenames ("struct "^ci.cname) (TComp(ci,[]));
        DoChildren
    | _ -> DoChildren
end

(*****************************************************************************)

type tau_sig =
    STVoid
  | STInt
  | STFloat
  | STPtr of tau_sig
  | STFun of tau_sig * tau_sig list
  | STComp of bool * string
  | STBuiltin_va_list
  | STLock
  | STAbs of tau_sig
  | STExists of tau_sig

type tau_t =
    ITVoid of vinfo option (* types flowing to and from void *)
  | ITInt of bool (* true if number is 0 (can be used as NULL) *)
  | ITFloat (* true if number is 0 (can be used as NULL) *)
  | ITTrylockInt of lock * bool (* true if not negated false if negated *)
  | ITPtr of tau ref * rho
  (*
    functions:
      1) a list of arguments (string: name of variable, tau: it's type),
      2) input phi
      3) input effect (really output-instantiated, because they flow backwards)
      4) return type
      5) output phi
      6) output effect (is input-instantiated)
   *)
  | ITFun of fdinfo

  (* struct/union *)
  | ITComp of cinfo
  | ITBuiltin_va_list of rho
  | ITLock of lock

  (* universal type.  can safely assume tau is a ITFun type.
   *)
  | ITAbs of tau ref

  | ITExists of existinfo

and tau = {
  t: tau_t;    (* the actual type structure *)
  ts: tau_sig; (* summary used as a hash *)
  tid: int;    (* unique identifier used to compare in O(1) *)
  tau_free_rho: rho option;  (* obsolete, used to lazily compute the free labels of a type *)
  tau_free_lock: lock option; (* obsolete, ditto *)
}

  (* existential types. tau usually is a ITComp, but so far I think
     the implementation is generic.
       --no it's not, because of the way we mark quantified stuff.
   *)
and existinfo = {
  mutable exist_tau: tau;
  exist_effect: effect;
  exist_phi: phi;
  mutable exist_abs: exp list;
  mutable exist_rhoset: rhoSet;
  mutable exist_lockset: lockSet;
  mutable exist_effectset: effectSet;
  mutable exist_initialized: bool;
}
and fdinfo = {
  mutable fd_arg_tau_list: (string * tau) list;
  fd_lock_effect : lock_effect;
  fd_input_phi: phi;
  fd_input_effect: effect;
  mutable fd_output_tau: tau;
  fd_output_phi: phi;
  fd_output_effect: effect;
  fd_epsilon: S.epsilon;
  fd_chi: chi;
}

and field_set = (rho * tau) StrHT.t

and compdata = {
  cinfo_id: int;
  compinfo: compinfo;

  (* used to print (semi) readable information about labels in the fields *)
  mutable cinfo_label_name : LN.label_name;

  mutable cinfo_loc: Cil.location;

  (* these are used in case this struct is conflated *)
  mutable cinfo_from_rho: rho option;
  mutable cinfo_to_rho: rho option;

  cinfo_fields: field_set;

  mutable cinfo_known: (tau_sig * tau) list;
  mutable cinfo_alloc: Cil.location list;
  mutable cinfo_inst_in_edges: cinfo InstHT.t;
  mutable cinfo_inst_out_edges: cinfo InstHT.t;
}

and voiddata_t =
  | ListTypes of (tau_sig * tau) list
  | ConflatedRho

and voiddata = {
    vinfo_id: int;
    vinfo_rho: rho option;      (* only used if void* conflation on *)
    vinfo_phi_in: phi option;   (* only used if void* conflation on *)
    vinfo_phi_out: phi option;  (* only used if void* conflation on *)
    mutable vinfo_loc: Cil.location;
    mutable vinfo_known: (tau_sig * tau) list;
    mutable vinfo_alloc: Cil.location list;
    mutable vinfo_inst_in_edges: vinfo InstHT.t;
    mutable vinfo_inst_out_edges: vinfo InstHT.t;

    mutable vinfo_types: voiddata_t;  (* types this void stands for *)
}

and cinfo = compdata U.uref
and vinfo = voiddata U.uref

type uniq = 
    UnqVar      (* a variable that is unique (both lval and storage) *)
  | UnqStorage  (* storage pointed to be a unique pointer *)
  | NotUnq      (* non-unique storage *)

type env = {
  goto_tbl : (stmt, gamma) Hashtbl.t;
  var_map : (tau * rho) Strmap.t;
  unpacked_map : existinfo Strmap.t;
}
and gamma = (env * phi * effect)

type special_function_t = exp list -> lval option -> (tau*uniq) list -> env ->
                          phi -> effect -> lock_effect -> gamma * S.epsilon

type global_kind =
    KGlobal       (* A global variable, always live *)
  | KMalloc_Addr  (* A malloc or address of a local, which have
                     self-loops but are not live, unless they
                     flow to a true global *)

(*****************************************************************************)
let get_vinfo_types : voiddata_t -> (tau_sig * tau) list =
function
  | ListTypes l -> l
  | ConflatedRho -> []
      (* the caller should test for conflation and handle it *)

module TauPair : Set.OrderedType with type t = (tau*tau) =
  struct
    type t = tau*tau
    let compare (x1,x2) (y1,y2) =
      if x1.tid < y1.tid then -1
      else if x1.tid > y1.tid then 1
      else if x2.tid < y2.tid then -1
      else if x2.tid > y2.tid then 1
      else 0
  end
module TauPairSet = Set.Make(TauPair)

module TauHT = Hashtbl.Make(
  struct
    type t = tau
    let equal t1 t2 = t1.tid = t2.tid
    let hash t = Hashtbl.hash t
  end)

module TauSet = Set.Make(
  struct
    type t = tau
    let compare t1 t2 = t1.tid - t2.tid
  end)

module InstEdge : Hashtbl.HashedType
  with type t = (tau*tau*instantiation*bool) =
  struct
    type t = tau*tau*instantiation*bool
    let equal (abs,inst,i,p) (abs',inst',i',p') =
      abs.tid = abs'.tid &&
      inst.tid = inst'.tid &&
      inst_equal i i' &&
      p = p'
    let hash (t1,t2,i,p) = 2 * t1.tid + t2.tid
  end

module InstEdgeTbl = Hashtbl.Make(InstEdge)

module CinfoInst : Hashtbl.HashedType with type t = (cinfo*instantiation) =
  struct
    type t = cinfo*instantiation
    let equal (c1,i1) (c2,i2) =
      (U.deref c1).cinfo_id = (U.deref c2).cinfo_id && (Inst.equal i1 i2)
    let hash (t,i) = (U.deref t).cinfo_id
  end
module CinfoInstHash = Hashtbl.Make(CinfoInst)

module VinfoInst : Hashtbl.HashedType with type t = (vinfo*instantiation) =
  struct
    type t = vinfo*instantiation
    let equal (c1,i1) (c2,i2) =
      (U.deref c1).vinfo_id = (U.deref c2).vinfo_id && (Inst.equal i1 i2)
    let hash (t,i) = (U.deref t).vinfo_id
  end
module VinfoInstHash = Hashtbl.Make(VinfoInst)

type labelsets = (rhoSet * lockSet * effectSet * phiSet)

(*****************************************************************************)

let thread_local_rhos : rhoSet ref = ref RhoSet.empty (* OBSOLETE *)

(* globals *)
let all_vinfo: vinfo list ref = ref []
let all_cinfo: cinfo list ref = ref []
let worklist_vinfo: vinfo Q.t = Q.create ()
let worklist_cinfo: cinfo Q.t = Q.create ()
let next_info: int ref = ref 1
let next_tau_id = ref 0
let undef_functions : string list ref = ref []
let def_functions : string list ref = ref []
let quantified_map : (string, exp) Hashtbl.t = Hashtbl.create 1
let global_var_tau : tau list ref = ref []  (* taus of global variables *)
let global_malloc_addr_tau : tau list ref= ref [] (* taus of mallocs and
                                                     (local) &vars *)
let global_var_rhos : RhoSet.t ref = ref RhoSet.empty (* rhos of global_var_tau *)
let global_var_locks : LockSet.t ref = ref LockSet.empty (* locks of global_var_tau *)
let inst_cinfo_hash : cinfo CinfoInstHash.t = CinfoInstHash.create 100
let inst_vinfo_hash : vinfo VinfoInstHash.t = VinfoInstHash.create 100
let inst_edges : unit InstEdgeTbl.t = InstEdgeTbl.create 100
let sub_edges : TauPairSet.t ref = ref TauPairSet.empty
let current_uniqueness : Uniq.state ref = ref Uniq.empty_state
(*let current_liveness : Lockdefs.Strset.t ref = ref Lockdefs.Strset.empty*)
let down_rules : (S.epsilon * env * Strset.t * S.epsilon) list ref = ref []
let eval_after_typing : (unit -> unit) Q.t = Q.create ()

let global_vars_computed = ref false
let get_global_var_rhos () =
  assert !global_vars_computed;
  !global_var_rhos

let get_global_var_locks () =
  assert !global_vars_computed;
  !global_var_locks

let clear_globals() : unit = begin
  all_vinfo := [];
  all_cinfo := [];
  undef_functions := [];
  def_functions := [];
  Hashtbl.clear quantified_map;
  CinfoInstHash.clear inst_cinfo_hash;
  VinfoInstHash.clear inst_vinfo_hash;
  InstEdgeTbl.clear inst_edges;
  global_var_tau := [];
  global_malloc_addr_tau := [];
  (* Do not clear global_var_rhos, global_var_locks *)
  sub_edges := TauPairSet.empty;
  thread_local_rhos := RhoSet.empty;
  current_uniqueness := Uniq.empty_state;
(*  current_liveness := Lockdefs.Strset.empty;*)
  down_rules := [];
end

type label =
  | Rho of rho
  | Lock of lock
  | Effect of effect
  | Phi of phi


let add_label_to_labelsets (l: label) (rs, ls, es, ps: labelsets)
                           : labelsets =
  match l with
    Rho(r) -> (RhoSet.add r rs), ls, es, ps
  | Lock(l) -> rs, (LockSet.add l ls), es, ps
  | Effect(e) -> rs, ls, (EffectSet.add e es), ps
  | Phi(p) -> rs, ls, es, (PhiSet.add p ps)

let get_top_label (t: tau) (e: exp) : label =
  let error = TypingBug
    "invalid expression in list of existentially quantified variables"
  in
  match t.t with
      ITVoid _
    | ITInt _
    | ITComp _
    | ITFloat
    | ITExists _ ->
        ignore(E.error "expression isn't pointer or lock: %a\n" d_exp e);
        raise error
    | ITBuiltin_va_list _
    | ITFun _
    | ITAbs _
    | ITTrylockInt _ ->
        raise (TypingBug "impossible")
    | ITPtr (_,r) ->
        Rho r
    | ITLock l ->
        Lock l

(* OBSOLETE *)
let fill_thread_local (env: env) : unit = begin
  thread_local_rhos := RhoSet.empty;
  Uniq.forall_unique
    (fun v -> 
      if !debug then ignore (E.log "%s is thread-local\n" v);
      let (t,r) = Strmap.find v env.var_map in
      thread_local_rhos := RhoSet.add r !thread_local_rhos;
      let rs =
        match t.t with
        | ITPtr (t,r) ->
            RhoSet.add r (
            match !t.t with
            | ITComp ci ->
                let c = U.deref ci in
                StrHT.fold
                  (fun fn (r,t) rs -> RhoSet.add r rs)
                  c.cinfo_fields
                  RhoSet.empty
            | _ -> RhoSet.empty
          )
        | ITAbs _ -> assert false
        | ITBuiltin_va_list _
        | ITFun _
        | ITTrylockInt _
        | ITVoid _
        | ITInt _
        | ITFloat
        | ITExists _
        | ITLock _ -> RhoSet.empty
        | ITComp ci ->
            let c = U.deref ci in
            StrHT.fold
              (fun fn (r,t) rs -> RhoSet.add r rs)
              c.cinfo_fields
              RhoSet.empty
      in
      thread_local_rhos := RhoSet.union rs !thread_local_rhos;
    ) !current_uniqueness;
end

let is_existential (s: string) (al: attribute list) : bool =
  let rec f = function
    [] -> true
  | (Attr("packed",[]))::_ -> false
  | _::tl -> f tl
  in
  Hashtbl.mem quantified_map s && f al

let is_atomic (v: varinfo) : bool = Cil.hasAttribute "atomic" v.vattr
  (*
  let rec f = function
    [] -> true
  | (Attr("atomic",[]))::_ -> false
  | _::tl -> f tl
  in
  f v.vattr
  *)

(*
let rec free_rhos (t: tau) (known: tau list) : rhoSet =
  if List.mem t known then RhoSet.empty else
  match t.t with
    ITVoid _
  | ITInt _
  | ITFloat
  | ITLock _
  | ITTrylockInt _ -> RhoSet.empty
  | ITPtr(tref, r) -> RhoSet.add r (free_rhos !tref (t::known))
  | ITFun fdinfo -> RhoSet.empty (* just being conservative *)
  | ITComp ci -> RhoSet.empty (* same *)
  | ITExists _ -> RhoSet.empty (* same *)
  | ITBuiltin_va_list _ -> RhoSet.empty
  | ITAbs _ -> assert false
*)

let uniq_deref u =
  match u with 
    UnqVar -> UnqStorage
  | UnqStorage -> NotUnq
  | NotUnq -> NotUnq
let safe_ignore u =
  match u with
    UnqVar | UnqStorage -> true
  | NotUnq -> false
let uniq2str u = 
  match u with
    UnqVar -> "Unique Variable"
  | UnqStorage -> "Unique Storage"
  | NotUnq -> "Shared"


let read_rho (r: rho) (p: phi) (e:effect) (u:uniq): unit =
  if !do_uniq && (safe_ignore u) then (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && not (RhoSet.mem r !thread_local_rhos) then 
      ignore(warn "NEW unique read of %a\n" d_rho r);
    if !debug then
      ignore(warn "removing thread-local read %a\n" d_rho r);
  ) else (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && (RhoSet.mem r !thread_local_rhos) then 
      ignore(warn "NO LONGER unique read of %a\n" d_rho r);
    add_to_read_effect r e;
    add_to_read_chi r !current_chi;
    Correlation.deref r p e
  )

let write_rho (r: rho) (p: phi) (e:effect) (u:uniq): unit =
  if !do_uniq && (safe_ignore u) then (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && not (RhoSet.mem r !thread_local_rhos) then 
      ignore(warn "MISSED unique write of %a\n" d_rho r);
    if !debug then
      ignore(warn "removing thread-local write %a\n" d_rho r);
  ) else (
    (* SANITY; thread_local_rhos is obsolete *)
    if !debug && (RhoSet.mem r !thread_local_rhos) then 
      ignore(warn "NO LONGER unique write of %a\n" d_rho r);
    add_to_write_effect r e;
    add_to_write_chi r !current_chi;
    Correlation.deref r p e
  )

let defer (f: unit -> unit) : unit =
  Q.add f eval_after_typing

let done_typing () =
  let rec loop () =
    let f = Q.take eval_after_typing in
    f();
    loop()
  in
  try loop() with Q.Empty -> () 



(*****************************************************************************)
(* pretty-printing *)

let string_of_cinfo (c: cinfo) =
  (U.deref c).compinfo.cname ^
  (if !debug_void then 
    "#" ^ string_of_int (U.deref c).cinfo_id
  else "") 

let rec d_sig () (ts: tau_sig) : doc =
  match ts with
    STVoid -> text "void"
  | STInt -> text "int"
  | STFloat -> text "float"
  | STPtr ts1 -> d_sig () ts1 ++ text "*"
  | STFun (ts1,tsl) ->
      d_sig () ts1 ++
      List.fold_left (fun d x -> d ++ text ", " ++ d_sig () x) (text "(") tsl
      ++ text ")"
  | STComp (s, n) ->
      (if s then text "struct " else text "union ") ++ text n
  | STBuiltin_va_list -> text "..."
  | STLock -> text "lock"
  | STAbs ts1 -> text "forall(" ++ d_sig () ts1 ++ text ")"
  | STExists (ts1) -> text "exists(" ++ d_sig () ts1 ++ text ")"

let rec d_siglist (): tau_sig list -> doc = function
    [] -> nil
  | h::[] -> begin
      d_sig () h
  end
  | h::tl -> begin
      d_sig () h ++ text ",\n" ++ d_siglist () tl
  end

let rec d_short_tau () (t: tau) : doc =
  match t.t with
    ITVoid _ -> text "void"
  | ITInt _ -> text "int"
  | ITFloat -> text "float"
  | ITTrylockInt _ -> text "trylockint"
  | ITPtr(tref,_) -> text "*" ++ d_short_tau () !tref
  | ITFun fi -> text "fun"
  | ITComp ci -> text ("struct " ^ (string_of_cinfo ci))
  | ITBuiltin_va_list _ -> text "va-list"
  | ITLock _ -> text "lock"
  | ITAbs tref -> text "forall " ++ d_short_tau () !tref
  | ITExists ei -> text "exists " ++ d_short_tau () ei.exist_tau

let field_set_to_fieldlist (f: field_set) =
    let l = StrHT.fold (fun fld v a -> (fld,v)::a) f [] in
    List.sort (fun (f1, _) (f2, _) -> (String.compare f1 f2)) l

let rec d_arglist (al: (string * tau) list) (known: TauSet.t) : doc =
  match al with
    [] -> nil
  | (n,h)::[] -> text (n^": ") ++ d_tau_r h known
  | (n,h)::tl -> text (n^": ") ++ d_tau_r h known ++ text ",\n" ++ d_arglist tl known

and d_fieldlist (fl: (string * (rho * tau)) list) (known: TauSet.t) : doc =
  match fl with
    [] -> nil
  | (n, (r,t))::[] ->
      dprintf "  <%a> %s: " d_rho r n ++ d_tau_r t known ++ line
  | (n, (r,t))::tl ->
      dprintf "  <%a> %s: " d_rho r n ++ d_tau_r t known ++ line ++ d_fieldlist tl known
(*
  | (n, (r,t))::[] ->
      line ++ align ++ dprintf "  <%s> %s: " d_rho r n ++ line ++ text "  " ++
        d_tau_r t known ++
      unalign
  | (n, (r,t))::tl ->
      line ++ align ++ dprintf "  <%s> %s: " d_rho r n ++ line ++ text "  " ++
        d_tau_r t known ++ line ++
      unalign ++ d_fieldlist tl known
*)

and d_tauset () ts : doc =
  let first = ref true in
  TauSet.fold
    (fun h d ->
      d ++ (if !first then (first := false; nil) else text ",\n")
      ++ d_tau_r h TauSet.empty)
    ts
    nil

and d_taulist () : tau list -> doc = function
    [] -> nil
  | h::[] -> begin
    d_tau_r h TauSet.empty;
  end
  | h::tl -> begin
    d_tau_r h TauSet.empty ++ text ",\n" ++ d_taulist () tl
  end

(*
and d_polytype (s: tau)
               (*z: zeta*)
               (l: lockSet)
               (r: rhoSet)
               (p: phiSet)
               (e: effectSet)
               (known: tau list) : doc =
  d_tau_r s known ++ dprintf "; %a %a %a %a)" d_lockset l d_rhoset r
                                              d_phiset p d_effectset e
  (*dprintf "; %s)" (dotstring_of_zeta z)*)
*)
and d_tau_r (t:tau) (known: TauSet.t) : doc =
  if TauSet.mem t known then d_sig () t.ts else
  match t.t with
  | ITVoid None  -> text "void"
  | ITVoid (Some vr)  ->
      let v = U.deref vr in
      if !debug then dprintf "(void#%d)" v.vinfo_id
      else text "(void)"
  | ITInt _ -> text "int"
  | ITFloat -> text "float"
  | ITTrylockInt(l,b) ->
      text (if b then "" else "!") ++
      dprintf "trylock-int(%a)" d_lock l
  | ITPtr(s,r) ->
      d_tau_r !s (TauSet.add t known) ++ dprintf "*^{%a}" d_rho r;
  | ITFun fi ->
        align ++
          text "(" ++
          align ++
            d_arglist fi.fd_arg_tau_list known ++
            text ",\n" ++ CF.d_phi () fi.fd_input_phi ++
          unalign ++ text ")\n->^{" ++
          align ++
            d_effect () fi.fd_input_effect ++ text ",\n" ++
            d_effect () fi.fd_output_effect ++ text "}" ++
          unalign ++ line ++
          text "(" ++
          align ++
            d_tau_r fi.fd_output_tau known ++ text ",\n" ++
            (CF.d_phi () fi.fd_output_phi) ++ text ")" ++
          unalign ++
        unalign
  | ITComp(c) ->
      if TauSet.mem t known then assert false (*dprintf "struct#%d %s" (U.deref c) .(string_of_cinfo c)*)
      else
        align ++ dprintf "struct %s {" (string_of_cinfo c) ++ line ++
          align ++
          d_fieldlist
            (field_set_to_fieldlist (U.deref c).cinfo_fields)
            (TauSet.add t known) ++
          unalign ++ text "}" ++ unalign
  | ITBuiltin_va_list(_) -> text "..."
  | ITLock(l) -> dprintf "lock(%a)" d_lock l
  | ITAbs s  ->
      text "(\\forall" ++ d_tau_r !s known ++ text ")"
  | ITExists ei ->
      dprintf "(\\exists [%a; %a] "
        (fun () x ->
          List.fold_left (fun s e -> s ++ d_exp () e) (text "") x)
          ei.exist_abs
        d_effect ei.exist_effect ++
      d_tau_r ei.exist_tau TauSet.empty ++ text ")"

let d_tau () (s: tau) : doc = d_tau_r s TauSet.empty

let d_env () (e: env) : doc =
  let d_entry (v: string) (s, r: tau * rho) (d: doc) : doc =
    d ++ text v ++ text " : " ++ d_tau_r s TauSet.empty ++ text "\n"
  in
  text "Gamma:" ++ align ++ line ++
       Strmap.fold d_entry e.var_map nil ++
       unalign ++ text "\n\n"

(*****************************************************************************)

let make_tau (t: tau_t) (ts: tau_sig) : tau =
  incr next_tau_id;
  { t = t;
    ts = ts;
    tid = !next_tau_id;
    tau_free_rho = None;
    tau_free_lock = None; }

let integer_tau = make_tau (ITInt false) STInt
let void_tau = make_tau (ITVoid None) STVoid

(*
let is_existential (al: attributes) : bool =
  let rec f = function
    [] -> false
  | (Attr("existential",[]))::_ -> true
  | _::tl -> f tl
  in
  f al
*)
  
let rec typsig_to_tau_sig (t: Cil.typsig) : tau_sig =
  if (List.mem t !locktypesigs) then STLock
  else
  match t with 
  | TSBase(TInt _) -> STInt
  | TSBase(TFloat _) -> STFloat 
  | TSBase(TVoid _) -> STVoid
  | TSBase(TBuiltin_va_list _) -> STBuiltin_va_list
  | TSEnum _ -> STInt
  | TSPtr (ts, _) -> begin
     (*
     match ts with
        TSComp(s,n,al) ->
          if (is_existential n al) then STPtr(STExists(STComp(s,n)))
          else STPtr (STComp(s,n))
      | _ ->
      *)
      STPtr (typsig_to_tau_sig ts)
    end
  | TSArray (t,l,_) -> STPtr(typsig_to_tau_sig t)
  | TSComp (s, n, al) -> STComp(s,n)
  | TSFun(rts,argts,_,_) ->
      STFun(typsig_to_tau_sig rts, 
      List.map (fun ts -> (typsig_to_tau_sig ts)) argts)
  | _ -> assert false

let typ_tau_sig (t: Cil.typ) : tau_sig =
  typsig_to_tau_sig (typeSigWithAttrs (fun x -> x) t)

(*
let make_optional_rho (name: LN.label_name) (concrete: bool): rho option =
  if !do_void_conflate || !do_void_single
  then Some (make_rho name concrete)
  else None
*)

let make_optional_phi_var (name: string): phi option =
  if !do_void_conflate || !do_void_single
  then Some (make_phi name PhiVar) else None

let make_vinfo (r_opt: rho option): vinfo = 
  if !debug_void then ignore(E.log "make_vinfo: #%d at %a\n" !next_info d_loc !Cil.currentLoc);
  let vd = U.uref {
    vinfo_id = !next_info;
    vinfo_rho = r_opt;
    vinfo_phi_in = make_optional_phi_var "conflated_in";
    vinfo_phi_out = make_optional_phi_var "conflated_out";
    vinfo_loc = !Cil.currentLoc;
    vinfo_known = [];
    vinfo_alloc = [];
    vinfo_inst_in_edges = InstHT.create 1;
    vinfo_inst_out_edges = InstHT.create 1;
    vinfo_types = if !do_void_conflate then ConflatedRho else ListTypes [];
  } in
  incr next_info;
  all_vinfo := vd::!all_vinfo;
  Q.add vd worklist_vinfo;
  vd

let make_cinfo (r: rho) (c: compinfo) (name: LN.label_name) =
  if !debug_void then ignore(E.log "make_cinfo: #%d at %a\n" !next_info d_loc !Cil.currentLoc);
  let ci = U.uref {
    compinfo = c;
    cinfo_label_name = LN.List(U.uref [name]);
    cinfo_fields = StrHT.create 0; (* assume many are empty *)
    cinfo_id = !next_info;
    cinfo_loc = !Cil.currentLoc;
    cinfo_known = [];
    cinfo_from_rho = None;
    cinfo_to_rho = None;
    cinfo_alloc = [];
    cinfo_inst_in_edges = InstHT.create 1;
    cinfo_inst_out_edges = InstHT.create 1;
  } in
  incr next_info;
  all_cinfo := ci::!all_cinfo;
  Q.add ci worklist_cinfo;
  ci
    
let mkEmptyExist ts : tau =
  let e = {
    exist_tau = integer_tau;
    exist_effect = LF.make_effect "exists" true;
    exist_phi = make_phi "exists" PhiVar;
    exist_abs = [];
    exist_rhoset = RhoSet.empty;
    exist_lockset = LockSet.empty;
    exist_effectset = EffectSet.empty;
    exist_initialized = false;
  } in
  make_tau (ITExists e) (STExists ts)


(*
let rec strip (s: tau) : tau_signature =
  match s with
  | ITVoid _  -> STVoid
  | ITInt _ -> STInt
  | ITFloat -> STFloat
  | ITTrylockInt _ -> STInt
  | ITPtr(s,_) -> STPtr(strip !s)
  | ITFun fi ->
      STFun (strip fi.fd_output_tau,
        List.map
          (fun (_, t) -> (strip t)) fi.fd_arg_tau_list)
  | ITComp(ci) ->
      let d = find_info_data ci in
      STComp(d.compinfo.cstruct, d.compinfo.cname)
  | ITBuiltin_va_list(_) -> STBuiltin_va_list
  | ITLock(l) -> STLock
  | ITAbs _ -> assert false
  | ITExists ei -> STExists(strip ei.exist_tau)
*)

(*****************************************************************************)
(* unify/join *)
let join_types (s1: tau) (s2: tau) : tau =
  if s1.tid = s2.tid then s1
  else (
    ignore(E.log "%a <-> %a\n" d_tau s1 d_tau s2);
    raise (TypingBug "not implemented 1")
  )

(*****************************************************************************)

(* If r is not in rs, then mark as global and additionally, if it is
   from a global variable, store in global_var_rhos *)
let mark_global_rho (r: rho) (k: global_kind) (rs: RhoSet.t)  : unit =
  if RhoSet.mem r rs then () else begin
    set_global_rho r;
    if k = KGlobal then global_var_rhos := RhoSet.add r !global_var_rhos
  end

(* If l is not in ls and it is from a global variable, then mark it as global
 * and store in global_var_locks.
 * If it is not from a global variable, it is in a malloc() and should not
 * be global
 *)
let mark_global_lock (l: lock) (k: global_kind) (ls: LockSet.t) : unit =
  if k = KGlobal && not (LockSet.mem l ls) then begin
    global_var_locks := LockSet.add l !global_var_locks;
    set_global_lock l;
  end

let known_globals = TauHT.create 0

let rec set_global_tau_r (t: tau)
                         (k: global_kind)
                         (quantified_labels: labelsets)
                         : unit =
  let qr, ql, qe, qp = quantified_labels in
  if TauHT.mem known_globals t then () else
  match t.t with
    ITVoid None -> ()
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITVoid (Some ur) -> begin
      let xd = U.deref ur in
      match xd.vinfo_types with
        ConflatedRho ->
          mark_global_rho (getSome xd.vinfo_rho) k qr;
          CF.set_global_phi (getSome xd.vinfo_phi_in) qp;
          CF.set_global_phi (getSome xd.vinfo_phi_out) qp;
      | ListTypes tl ->
          List.iter (fun (_, t) -> set_global_tau_r t k quantified_labels) tl
    end
  | ITTrylockInt _ -> assert false (* never happens *)
  | ITPtr(tref, r) ->
      TauHT.add known_globals t ();
      mark_global_rho r k qr;
      set_global_tau_r !tref k quantified_labels
  | ITFun fi ->
      TauHT.add known_globals t ();
      List.iter
        (fun (_,argt) -> set_global_tau_r argt k quantified_labels)
        fi.fd_arg_tau_list;
      set_global_tau_r fi.fd_output_tau k quantified_labels;
      CF.set_global_phi fi.fd_input_phi qp;
      CF.set_global_phi fi.fd_output_phi qp;
      set_global_effect fi.fd_input_effect qe;
      set_global_effect fi.fd_output_effect qe;
      set_global_chi fi.fd_chi;
      S.epsilon_global fi.fd_epsilon;
  | ITLock(l) ->
      if !debug then ignore(E.log "lock %a is global\n" d_lock l);
      mark_global_lock l k ql
  | ITComp(ci) ->
      TauHT.add known_globals t ();
      let xd = U.deref ci in
      StrHT.iter
        (fun f (r,u) ->
          mark_global_rho r k qr;
          set_global_tau_r u k quantified_labels)
        xd.cinfo_fields;
  | ITBuiltin_va_list(r) ->
      mark_global_rho r k qr
  | ITAbs _ -> assert false
  | ITExists ei ->
      TauHT.add known_globals t ();
      let qset = (
        RhoSet.union qr ei.exist_rhoset,
        LockSet.union ql ei.exist_lockset,
        EffectSet.union qe ei.exist_effectset,
        qp) in
      set_global_tau_r ei.exist_tau k qset
(*end*)

let empty_labelsets : labelsets =
  (RhoSet.empty, LockSet.empty, EffectSet.empty, PhiSet.empty)

(* Stores t in either global_var_tau or global_malloc_addr_tau,
   depending on value of k *)
let mark_global_tau (t: tau) (k: global_kind) : unit = begin
  match (t.t, k) with
    (ITAbs tref, _) -> () (* set_global_tau_r !tref [] *)
  | (_, KGlobal) -> global_var_tau := t::(!global_var_tau)
  | (_, KMalloc_Addr) -> global_malloc_addr_tau := t::(!global_malloc_addr_tau)
end

let set_globals () : unit = begin
  TauHT.clear known_globals;
  (** Mark self-loops on global variables, and add to global_var_rho
     and global_var_lock as appropriate **)
  let max = List.length !global_var_tau in
  let count = ref 0 in
  if !debug then
    ignore(E.log "Setting global variables, length=%d\n" max);
   List.iter
    (fun t ->
      count := !count + 1;
      if !debug then
        ignore(E.log "setting global var %d/%d %s\n" !count max (Lprof.timestamp ()));
      set_global_tau_r t KGlobal empty_labelsets)
    !global_var_tau;
  global_var_tau := [];
  (** Mark self-loops on mallocs and &local variables **)
  let max = List.length !global_malloc_addr_tau in
  let count = ref 0 in
  if !debug then
    ignore(E.log "Setting malloc/addr variables, length=%d\n" max);
   List.iter
    (fun t ->
      count := !count + 1;
      if !debug then
        ignore(E.log "setting malloc/addr var %d/%d %s: %a\n" !count max (Lprof.timestamp ()) d_short_tau t);
      set_global_tau_r t KMalloc_Addr empty_labelsets)
    !global_malloc_addr_tau;
  global_malloc_addr_tau := [];
  (** Cleanup **)
  global_vars_computed := true;
  TauHT.clear known_globals;
  if !debug then ignore (E.log "Done %s\n" (Lprof.timestamp ()));
  if !debug then
    begin
      RhoSet.iter
        (fun r -> ignore(E.log "global var rho: %a\n" d_rho r))
        (get_global_var_rhos ());
      LockSet.iter
        (fun l -> ignore(E.log "global var lock: %a\n" d_lock l))
        (get_global_var_locks ());
    end
end

(* get a type and "allocate" it.
 * creates a constant R that flows to every \rho in t, excluding \rhos under
 * pointers.
 *)
let rec allocate (t: tau) : unit = begin
  match t.t with
  | ITBuiltin_va_list _ -> ()
  | ITVoid None
  | ITTrylockInt _ 
  | ITAbs _ ->
      assert false (* these should never happen *)
  | ITExists _ ->
      let d = dprintf "%a: trying to allocate existential type" Cil.d_loc !Cil.currentLoc in
      raise (TypingBug (string_of_doc d))
  | ITVoid (Some ur) ->
      let u = (U.deref ur) in
      u.vinfo_alloc <- !Cil.currentLoc ::u.vinfo_alloc;
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITPtr(_, _) -> () (* pointer allocation doesn't allocate a value for it *)
  | ITFun _ -> assert false (* allocating a function?! *)
  | ITLock(_) -> ()
  | ITComp ci  ->
      let c = (U.deref ci) in
      c.cinfo_alloc <- !Cil.currentLoc::c.cinfo_alloc;
end

let rec visit_concrete_tau (f: rho -> unit) (t: tau) : unit = begin
  match t.t with
  | ITBuiltin_va_list _ -> ()
  | ITVoid None
  | ITTrylockInt _ 
  | ITAbs _ -> assert false (* these should never happen *)
  | ITExists _ ->
      let d = dprintf "%a: dereference of existential labels outside unpack" Cil.d_loc !Cil.currentLoc in
      raise (TypingBug (string_of_doc d))
  | ITVoid (Some ur) ->
      defer
        (fun () ->
          let u = (U.deref ur) in
          match u.vinfo_types with
            ConflatedRho -> f (getSome u.vinfo_rho)
          | ListTypes tl ->
              List.iter (fun (_,t) -> visit_concrete_tau f t) tl
        )
  | ITInt b -> assert (not b)
  | ITFloat -> ()
  | ITPtr(_, _) -> ()
  | ITFun _ -> assert false (* allocating a function?! *)
  | ITLock(_) -> ()
  | ITComp ci  ->
      defer
        (fun () ->
          let c = (U.deref ci) in
          StrHT.iter
            (fun s (r,t) ->
              f r;
              visit_concrete_tau f t)
            c.cinfo_fields 
        )
end

(*****************************************************************************)
(* env utils *)
let fresh_env () = {
  goto_tbl = Hashtbl.create 1;
  var_map = Strmap.empty;
  unpacked_map = Strmap.empty;
}

let env_add_var (e: env) (varname: string) (a: tau * rho) : env =
  { e with var_map = Strmap.add varname a e.var_map; }

let env_add_unpack_ei (e: env) (name: string) (ei: existinfo) (a: tau * rho) : env =
  assert (not (Strmap.mem name e.unpacked_map));
  { e with
    var_map = Strmap.add name a e.var_map;
    unpacked_map = Strmap.add name ei e.unpacked_map; }

let env_del_unpack_ei (e: env) (name: string) : env =
  assert (Strmap.mem name e.unpacked_map);
  { e with unpacked_map = Strmap.remove name e.unpacked_map; }

let global_env : env ref = ref (fresh_env())

let env_lookup (varname: string) (e: env) : (tau * rho) =
  try
    Strmap.find varname e.var_map
  with Not_found ->
    Strmap.find varname !global_env.var_map

let rec get_free_vars t free_vars known =
  if TauSet.mem t known then free_vars else
  let free_locks, free_rhos = free_vars in
  match t.t with
  | ITPtr (tref,r) ->
      if RhoSet.mem r free_rhos then free_vars else
      let rs = RhoSet.add r free_rhos in
      get_free_vars !tref (free_locks, rs) (TauSet.add t known)
  | ITBuiltin_va_list r -> free_locks, RhoSet.add r free_rhos
  | ITTrylockInt _
  | ITInt _
  | ITVoid None
  | ITFloat
  | ITAbs _ -> free_vars
  | ITVoid (Some vi) -> begin
      let v = U.deref vi in
      match v.vinfo_types with
        ConflatedRho ->
          free_locks, RhoSet.add (getSome v.vinfo_rho) free_rhos
      | ListTypes tl ->
          List.fold_left
            (fun vars (_, t') -> get_free_vars t' vars (TauSet.add t known))
            free_vars
            tl
    end
  | ITComp ci ->
      let c = U.deref ci in
      StrHT.fold
        (fun _ (r,t') (fl,fr) ->
          get_free_vars t' (fl,RhoSet.add r fr) (TauSet.add t known))
        c.cinfo_fields
        free_vars
  | ITFun fdi -> 
      List.fold_left
        (fun fv (_,t') -> get_free_vars t' fv (TauSet.add t known))
        (get_free_vars fdi.fd_output_tau free_vars (TauSet.add t known))
        fdi.fd_arg_tau_list
  | ITExists ei ->
        let l1,r1 =
          get_free_vars ei.exist_tau (LockSet.empty,RhoSet.empty)
          (TauSet.add t known)
        in
        let l2 = LockSet.diff l1 ei.exist_lockset in
        let r2 = RhoSet.diff r1 ei.exist_rhoset in
        (LockSet.union l2 free_locks, RhoSet.union r2 free_rhos)
  | ITLock l -> LockSet.add l free_locks, free_rhos

let find_free_vars env =
  Strmap.fold
    (fun varname (t,r) (fl,fr) ->
       let fr' = RhoSet.add r fr in
       get_free_vars t (fl,fr') TauSet.empty)
    env.var_map
    (LockSet.empty, RhoSet.empty)

let find_live_vars env live_vars =
  Strset.fold
    (fun varname (fl,fr) ->
      let t,r = env_lookup varname env in
      let fr' = RhoSet.add r fr in
      if !debug then ignore(E.log "live at down: %s\n" varname);
      get_free_vars t (fl,fr') TauSet.empty)
    live_vars
    (LockSet.empty, RhoSet.empty)

(* This function applies (Down), filtering out allocation effects
   (S.epsilon) that refer to variables not in scope.  We don't
   generate the constraints right away, since we need to wait until
   lazy structs have been calculated. *)
let down e1 env live_vars e2 =
  if !do_down then
    defer (fun () ->
      let ls,rs = find_live_vars env live_vars in
      let ls' = LockSet.union ls (get_global_var_locks ()) in
      let rs' = RhoSet.union rs (get_global_var_rhos ()) in
      if !debug then ignore(E.log "down filters on: %a\n" d_lockset ls');
      S.epsilon_filter e1 (ls',rs') e2)
    (*down_rules := (e1,env,live_vars,e2)::!down_rules*)
  else S.epsilon_flow e1 e2

(*
let apply_down () =
  List.iter
    (fun (e1,env,live_vars,e2) ->
      let ls,rs = find_live_vars env live_vars in
      let ls' = LockSet.union ls (get_global_var_locks ()) in
      let rs' = RhoSet.union rs (get_global_var_rhos ()) in
      if !debug then ignore(E.log "down filters on: %a\n" d_lockset ls');
      S.epsilon_filter e1 (ls',rs') e2)
    !down_rules
*)


(*****************************************************************************)
let is_goto_target (s: stmt) : bool =
  match s.labels with
    [] -> true
  | _ -> false

let rec unify_env (e1: env) (e2: env) : unit =
  if e1 == e2 then () else begin
    (* only join envs from the same context: *)
    assert (e1.goto_tbl == e2.goto_tbl);
    (*assert (e1.unpacked_map = e2.unpacked_map);*)
    assert (Strmap.equal
      (fun (t1,r1) (t2,r2) -> unify_types t1 t2; unify_rho r1 r2; true)
      e1.var_map e2.var_map);
  end

and env_flows (e1: env) (e2: env) : unit =
  if e1 == e2 then () else begin
    assert (e1.goto_tbl == e2.goto_tbl);
    assert (e1.unpacked_map = e2.unpacked_map);
    assert (Strmap.equal
      (fun (t1,r1) (t2,r2) -> sub_tau t1 t2; rho_flows r1 r2; true)
      e1.var_map e2.var_map);
  end
  
and join_gamma (env1,phi1,eff1: gamma) (env2,phi2,eff2: gamma) : gamma =
  unify_env env1 env2;
  (env1, join_phi phi1 phi2 PhiVar, join_effects eff1 eff2)

(* goto utils *)
and set_goto_target (e: env) (p: phi) (eff: effect) (s: stmt) : unit =
  try
    let (e',p',eff') = Hashtbl.find e.goto_tbl s in
    unify_env e e';
    effect_flows eff' eff;
    CF.phi_flows p p';
  with Not_found -> begin
    let p' = make_phi "label" PhiVar in
    let eff' = make_effect "e" false in
    Hashtbl.add e.goto_tbl s (e, p', eff');
    effect_flows eff' eff;
    CF.phi_flows p p';
  end

and reannotate (t: tau)
               (known: (tau_sig * tau) list)
               (name: LN.label_name) (* used to create readable names for labels *)
               : tau =
  let result =
  if List.mem_assoc t.ts known
  then List.assoc t.ts known
  else begin
    match t.t with
      ITVoid None -> t
    | ITVoid (Some ur) -> assert false
    | ITPtr(tref, _) -> begin
        (*if List.mem_assq !tref known
        then List.assq !tref known else*)
        match (!tref).t with
          ITVoid (Some _) ->
            (* Make a new empty void* *)
            let r = make_rho name false in
            let ropt =
              if (!do_void_conflate || !do_void_single)
              then Some r
              else None
            in
            let vi = make_vinfo ropt in
            let t1 = make_tau (ITVoid (Some vi)) (!tref).ts in
            let rt = make_tau (ITPtr(ref t1, r)) t.ts in
            assert ((U.deref vi).vinfo_known = []);
            (U.deref vi).vinfo_known <- (t.ts, rt)::known;
            rt
        | _ ->
            let newtref = ref (make_tau (ITInt false) (!tref).ts) in
            let newptrt = make_tau (ITPtr(newtref, make_rho name false)) t.ts in
            newtref := reannotate !tref ((*(t.ts, newptrt)::*)known) (LN.Deref name);
            newptrt
      end
    | ITInt b -> assert (not b); t
    | ITFloat -> t
    | ITTrylockInt _ -> assert false (* never happens *)
    | ITFun fi ->
        let inphi = make_phi "in" PhiVar in
        let ineff = LF.make_effect "reannot-in" false in
        let newfi = {
          fd_arg_tau_list = [];
          fd_lock_effect = make_lock_effect ();
          fd_input_phi = inphi;
          fd_input_effect = ineff;
          fd_output_tau = make_tau (ITInt false) fi.fd_output_tau.ts;
          fd_output_phi = make_phi "out" PhiVar;
          fd_output_effect = LF.make_effect "reannot-out" false;
          fd_epsilon = S.make_var_epsilon ();
          fd_chi = make_chi "X";
        } in
        let newt = make_tau (ITFun newfi) t.ts in
        let argtypes =
          List.map
            (fun (argname, argt) ->
              let t = reannotate argt ((*(t.ts,newt)::*)known) (LN.Field (name, argname)) in
              let r = (argname, t) in r)
            fi.fd_arg_tau_list
        in
        let newret =
          reannotate fi.fd_output_tau ((*(t.ts,newt)::*)known) (LN.Field (name,"return")) in
        newfi.fd_output_tau <- newret;
        newfi.fd_arg_tau_list <- argtypes;
        newt
    | ITLock(_) -> make_tau (ITLock(make_lock name false)) t.ts
    | ITComp(c1) ->
        let r = make_rho name false in
        let c2 = make_cinfo r (U.deref c1).compinfo name in
        let rt = make_tau (ITComp(c2)) t.ts in
        (U.deref c2).cinfo_known <- (t.ts,rt)::known;
        rt
    | ITBuiltin_va_list(r) ->
        make_tau (ITBuiltin_va_list(make_rho (LN.Const "va_list") false)) t.ts
    | ITAbs _ -> assert false (* never happens *)
    | ITExists ei -> begin
        let newt = mkEmptyExist ei.exist_tau.ts in
        let newtpacked =
          reannotate ei.exist_tau ((t.ts,newt)::known) name in
        fillExist newt newtpacked;
        newt
      end
  end
  in
  (*
  ignore(E.log "reannotate returns: %a : %a\n" d_sig result.ts d_tau result);
  *)
  result


(*****************************************************************************)
and annotate (t: typ)
             (known: (tau_sig * tau) list)
             (name: LN.label_name)
             : tau =
  let ts = typ_tau_sig t in
  if List.mem_assoc ts known
  then List.assoc ts known
  else
  if
    let t = Cil.unrollTypeDeep t in
    try (List.memq t !locktypes)
    with Not_found -> false
  then make_tau (ITLock(make_lock name false)) STLock
  else
  let result = (
  match t with
    TVoid _  -> void_tau
  | TPtr(tptd,_)
  | TArray(tptd, _, _) -> begin
      let tsptr = typ_tau_sig tptd in
      if !do_compact_structs && (List.mem_assoc tsptr known)
      then
        let rtref = ref (List.assoc tsptr known) in
        make_tau (ITPtr(rtref, make_rho name false)) (STPtr tsptr)
      else begin
        match tptd with
          TVoid(_) ->
            let r = make_rho name false in
            let ropt =
              if (!do_void_conflate || !do_void_single)
              then Some r
              else None
            in
            let vi = make_vinfo ropt in
            let tt = make_tau (ITVoid (Some vi)) tsptr in
            let rt = make_tau (ITPtr(ref tt, r)) ts in
            assert ((U.deref vi).vinfo_known = []);
            (U.deref vi).vinfo_known <- (ts,rt)::known;
            rt
        | TComp(c, al) ->
            let r = make_rho name false in
            let cinfo = make_cinfo r c (LN.Deref name) in
            let rt1 = make_tau (ITComp cinfo) tsptr in
            let rt, ts =
              if (is_existential c.cname al) then (
                let t = mkEmptyExist rt1.ts in
                fillExist t rt1;
                t, (STExists tsptr)
               ) else rt1, tsptr
            in
            (U.deref cinfo).cinfo_known <- (ts,rt)::known;
            let pt = make_tau (ITPtr(ref rt, r)) (STPtr ts) in
            pt
        | TNamed(ti, attr) ->
            let b =
              try
                List.memq (Hashtbl.find typenames ti.tname) !locktypes
              with Not_found -> false
            in
            if (List.mem ti.tname !lock_type_names || b)
            then
              let tref = make_tau (ITLock(make_lock (LN.Deref name) false)) STLock in
              make_tau (ITPtr(ref tref, make_rho name false)) (STPtr STLock)
            else
              annotate (TPtr (Cil.typeAddAttributes attr ti.ttype, [])) known (LN.Deref name)
        | _ ->
            let rtptr = make_tau (ITInt false) tsptr in
            let rtref = ref rtptr in
            let rt = make_tau (ITPtr(rtref, make_rho name false)) ts in
            rtref := annotate tptd ((*(ts, rt)::*)known) (LN.Deref name);
            rt
      end
  end
  | TInt(_,_) -> integer_tau
  | TFloat(_,_) -> make_tau ITFloat ts
  | TFun(tret, args, b, _) ->
      let fi = {
        fd_arg_tau_list = [];
        fd_lock_effect = make_lock_effect ();
        fd_input_phi = make_phi "in" PhiVar;
        fd_input_effect = LF.make_effect "inputeffect" false;
        fd_output_tau = make_tau (ITInt false) (typ_tau_sig tret);
        fd_output_phi = make_phi "out" PhiVar;
        fd_output_effect = LF.make_effect "outputeffect" false;
        fd_epsilon = S.make_var_epsilon ();
        fd_chi = make_chi "X";
      } in
      let fun_tau = make_tau (ITFun fi) ts in
      let arglist =
        match args with
          None -> [("",TVoid([]),[])]
        | Some(l) -> l
      in
      let arglist =
        if b then arglist@[("",TBuiltin_va_list([]),[])] else arglist
      in
      (*let known = (ts, fun_tau)::known in*)
      let i = ref 1 in
      let argtypes =
        List.map
          (fun (s, ta, _) ->
            let s = if s = "" then sprintf "arg%d" !i else s in
            incr i;
            let r = (s, annotate ta known (LN.Field(name, s))) in
            r
          )
          arglist
      in
      let ot = annotate tret known (LN.Field (name, "return")) in
      fi.fd_output_tau <- ot;
      fi.fd_arg_tau_list <- argtypes;
      fun_tau
  | TNamed(ti, attr) ->
      let b =
        try
          List.memq (Hashtbl.find typenames ti.tname) !locktypes
        with Not_found -> false
      in
      if (List.mem ti.tname !lock_type_names || b)
      then make_tau (ITLock(make_lock name false)) STLock
      else annotate ti.ttype known name
  | TComp(c, al) ->
      let r = make_rho (LN.AddrOf name) false in
      let cinfo = make_cinfo r c name in
      let rt1 = make_tau (ITComp cinfo) ts in
      let rt, ts = rt1, ts in
      (U.deref cinfo).cinfo_known <- (ts,rt)::known;
      rt
  | TEnum(_,_) -> integer_tau
  | TBuiltin_va_list(_) -> make_tau (ITBuiltin_va_list(make_rho (LN.Const "va_list") false)) ts
  ) in
  (*
  ignore(E.log "annotate returns: %a : %a\n" d_sig result.ts d_tau result);
  *)
  result


and inst_vinfo (vi1: vinfo) (vi2: vinfo) (i: instantiation) : unit =
  begin
    let v1 = U.deref vi1 in
    let v2 = U.deref vi2 in
    if !debug_void then ignore(E.log "inst_vinfo: #%d to #%d on %a\n" v1.vinfo_id v2.vinfo_id d_instantiation i);
    if !do_void_conflate || !do_void_single then begin
      inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) true i;
      inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) false i;
      CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) true i;
      CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) false i;
      CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) true i;
      CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) false i;
    end;
    try
      let v = InstHT.find v1.vinfo_inst_out_edges i in
      U.unify unify_vinfo (vi2, v)
    with Not_found -> InstHT.add v1.vinfo_inst_out_edges i vi2;
    try
      let v = InstHT.find v2.vinfo_inst_in_edges i in
      U.unify unify_vinfo (vi1, v)
    with Not_found -> InstHT.add v2.vinfo_inst_in_edges i vi1;
  end

(*****************************************************************************)
and unify_vinfo (src, tgt: voiddata*voiddata) : voiddata =
  let src, tgt =
    if (src.vinfo_id < tgt.vinfo_id) then src, tgt else tgt, src
  in
  if (src.vinfo_id = tgt.vinfo_id) then src
  else begin
  if !debug_void then ignore(E.log "unify_vinfo: #%d with #%d\n" src.vinfo_id tgt.vinfo_id);
  (*
    let src, tgt =
      if (List.length x.vinfo_types < List.length y.vinfo_types) then x,y else y,x
    in
  *)
    InstHT.iter
      (fun i vi ->
        try
          let v = InstHT.find tgt.vinfo_inst_in_edges i in
          U.unify unify_vinfo (vi, v)
        with Not_found -> InstHT.add tgt.vinfo_inst_in_edges i vi)
      src.vinfo_inst_in_edges;
    InstHT.iter
      (fun i vi ->
        try
          let v = InstHT.find tgt.vinfo_inst_out_edges i in
          U.unify unify_vinfo (vi, v)
        with Not_found -> InstHT.add tgt.vinfo_inst_out_edges i vi)
      src.vinfo_inst_out_edges;
    InstHT.clear src.vinfo_inst_in_edges;
    InstHT.clear src.vinfo_inst_out_edges;
    tgt.vinfo_alloc <- src.vinfo_alloc @ tgt.vinfo_alloc;

    if !do_void_conflate || !do_void_single then begin
      unify_rho (getSome src.vinfo_rho) (getSome tgt.vinfo_rho);
      CF.unify_phi (getSome src.vinfo_phi_in) (getSome tgt.vinfo_phi_in);
      CF.unify_phi (getSome src.vinfo_phi_out) (getSome tgt.vinfo_phi_out);
    end;
    begin
      tgt.vinfo_known <- src.vinfo_known @ tgt.vinfo_known;
      src.vinfo_known <- [];
      List.iter
        (fun (ts, t) ->
          let tgtlist = (get_vinfo_types tgt.vinfo_types) in
          (if !do_void_single then assert (List.length tgtlist <= 1));
          try
            let t2 = List.assoc ts tgtlist in
            unify_types t t2
          with Not_found -> begin
            add_type_to_vinfo t tgt
          end
        )
        (get_vinfo_types src.vinfo_types);
    end;
    tgt
  end

and inst_cinfo (ci1: cinfo) (ci2: cinfo) (i: instantiation) : unit =
  begin
    let c1 = U.deref ci1 in
    let c2 = U.deref ci2 in
    if !debug_void then ignore(E.log "inst_cinfo: #%d to #%d on %a\n" c1.cinfo_id c2.cinfo_id d_instantiation i);
    try
      let c = InstHT.find c1.cinfo_inst_out_edges i in
      U.unify unify_cinfo (ci2, c)
    with Not_found -> InstHT.add c1.cinfo_inst_out_edges i ci2;
    try
      let c = InstHT.find c2.cinfo_inst_in_edges i in
      U.unify unify_cinfo (ci1, c)
    with Not_found -> InstHT.add c2.cinfo_inst_in_edges i ci1;
  end

and unify_cinfo (x, y: compdata*compdata) : compdata =
  if !debug_void then ignore(E.log "unify_cinfo: #%d with #%d\n" x.cinfo_id y.cinfo_id);
  assert (x.compinfo == y.compinfo);
  if (x.cinfo_id = y.cinfo_id) then x
  else begin
    let src,tgt = if (x.cinfo_id < y.cinfo_id) then y,x else x,y in
    (*let x,y = (0,0) in (* don't use x, y below here! *)*)
    InstHT.iter
      (fun i ci ->
        try
          let c = InstHT.find tgt.cinfo_inst_in_edges i in
          U.unify unify_cinfo (ci, c)
        with Not_found -> InstHT.add tgt.cinfo_inst_in_edges i ci)
      src.cinfo_inst_in_edges;
    InstHT.iter
      (fun i ci ->
        try
          let c = InstHT.find tgt.cinfo_inst_out_edges i in
          U.unify unify_cinfo (ci, c)
        with Not_found -> InstHT.add tgt.cinfo_inst_out_edges i ci)
      src.cinfo_inst_out_edges;
    InstHT.clear src.cinfo_inst_in_edges;
    InstHT.clear src.cinfo_inst_out_edges;
    tgt.cinfo_known <- src.cinfo_known @ tgt.cinfo_known;
    tgt.cinfo_alloc <- src.cinfo_alloc @ tgt.cinfo_alloc;
    src.cinfo_known <- [];
    (match tgt.cinfo_label_name, src.cinfo_label_name with
      LN.List lu1, LN.List lu2 -> U.unify (fun (l1,l2) -> l1 @ l2) (lu1, lu2)
    | _ -> assert false);
    begin
      match src.cinfo_to_rho, tgt.cinfo_to_rho with
        None, None -> ()
      | Some r, None
      | None, Some r -> tgt.cinfo_to_rho <- Some r
      | Some r1, Some r2 -> unify_rho r1 r2
    end;
    begin
      match src.cinfo_from_rho, tgt.cinfo_from_rho with
        None, None -> ()
      | Some r, None
      | None, Some r -> tgt.cinfo_from_rho <- Some r
      | Some r1, Some r2 -> unify_rho r1 r2
    end;
    StrHT.iter
      (fun f (r,t) ->
        try
          let (r2,t2) = StrHT.find tgt.cinfo_fields f in
          unify_types t t2;
          unify_rho r r2;
        with Not_found -> begin
          StrHT.add tgt.cinfo_fields f (r,t);
        end
      ) src.cinfo_fields;
    tgt
  end

and unify_cinfo_inst (x, y: field_set*field_set) : field_set =
  (* Put entries from small table into large *)
  let small,large = if StrHT.length x < StrHT.length y then x,y else y,x in
  begin
    StrHT.iter 
      (fun fld v -> (StrHT.replace large fld v))
      small;
    large
  end

and sub_tau (src: tau) (tgt: tau) : unit =
  let rec sub_tau1 (src: tau)
                   (tgt: tau)
                   (locopt: (rho * rho) option)
                   : unit =
    if src == tgt then ()
    else if TauPairSet.mem (src,tgt) !sub_edges then ()
    else begin
      sub_edges := TauPairSet.add (src,tgt) !sub_edges;
      match (src.t,tgt.t) with
        ITVoid None, ITVoid None -> ()
      | ITVoid(Some fromvi), ITVoid(Some tovi) ->
          U.unify unify_vinfo (fromvi, tovi);
      | ITFloat, ITFloat -> ()
      | ITFloat, ITInt _ -> ()
      | ITInt _, ITFloat -> ()
      | ITTrylockInt _, ITInt b
      | ITInt _, ITInt b -> assert (not b)
      | ITInt _, ITTrylockInt _ ->
          raise  (TypingBug "overwritting trylock-result")
      | ITInt b, ITPtr _ ->
          if (not b) then ignore(warn "assigning number to pointer")
      | ITPtr(tref1, r1), ITPtr(tref2, r2) -> begin
          rho_flows r1 r2;
          match (!tref1).t, (!tref2).t with
            ITVoid(Some fromvi), ITVoid(Some tovi) ->
              U.unify unify_vinfo (fromvi, tovi);
          | ITVoid(Some vi), nonvoid ->
              add_type_to_vinfo tgt (U.deref vi);
          | nonvoid, ITVoid(Some vi) ->
              add_type_to_vinfo src (U.deref vi);
          | _ ->
            sub_tau1 !tref1 !tref2 (Some(r1, r2));
            sub_tau1 !tref2 !tref1 (Some(r2, r1));
        end

      | ITBuiltin_va_list(r1), ITBuiltin_va_list(r2) ->
          rho_flows r1 r2;

      | ITBuiltin_va_list(r), _
      | _, ITBuiltin_va_list(r) ->
          ignore(E.log "%a -> %a\n" d_tau src d_tau tgt);
          assert false

      | ITPtr(tref1, r1), _ -> begin
          match locopt with
            None ->
              if !debug then
                ignore(warn "assigning pointer to non-pointer: %a\n ->\n %a"
                       d_tau src d_tau tgt
                )
          | Some(srcr,tgtr) ->
              rho_flows r1 tgtr;
              let phi_in = make_phi "conflated_in" PhiVar in
              let phi_out = make_phi "conflated_out" PhiVar in
              conflate_to !tref1 tgtr phi_in phi_out TauSet.empty;
        end
      | _, ITPtr(tref2, r2) -> begin
          match locopt with
            None ->
              ignore(warn "assigning pointer to non-pointer: %a\n ->\n %a"
                     d_tau src d_tau tgt
              )
          | Some(srcr,tgtr) ->
              rho_flows srcr r2;
              let phi_in = make_phi "conflated_in" PhiVar in
              let phi_out = make_phi "conflated_out" PhiVar in
              conflate_from srcr !tref2 phi_in phi_out TauSet.empty;
        end
      | ITFun fi1, ITFun fi2 ->
          CF.phi_flows fi2.fd_input_phi fi1.fd_input_phi;
          effect_flows fi1.fd_input_effect fi2.fd_input_effect;
          CF.phi_flows fi1.fd_output_phi fi2.fd_output_phi;
          effect_flows fi2.fd_output_effect fi1.fd_output_effect;
          lock_effect_flows fi1.fd_lock_effect fi2.fd_lock_effect;
          sub_tau1 fi1.fd_output_tau fi2.fd_output_tau None;
          S.epsilon_flow fi1.fd_epsilon fi2.fd_epsilon;
          (*chi_flows fi2.fd_chi fi1.fd_chi;*)
          chi_flows fi1.fd_chi fi2.fd_chi;
          let f = (fun (_,s) -> s) in
          let flist1 = List.map f fi1.fd_arg_tau_list in
          let flist2 = List.map f fi2.fd_arg_tau_list in
          sub_tau_list flist2 flist1 None;
      | ITComp(ci1), ITComp(ci2) ->
          if (U.deref ci1).compinfo == (U.deref ci2).compinfo
          then U.unify unify_cinfo (ci1, ci2)
          else (
            match locopt with
              None -> 
                ignore(E.error "assigning concrete structs of different type!")
            | Some(r1,r2) -> begin
                let phi_in = make_phi "conflated_in" PhiVar in
                let phi_out = make_phi "conflated_out" PhiVar in
                conflate src r2 phi_in phi_out TauSet.empty;
              end
          )
          
      (* from all fields of a union to a type *)
      | ITLock(l1), ITLock(l2) -> unify_locks l1 l2
      | ITLock _, _ -> ignore(warn "assignment from lock to non-lock type |%a|" d_tau tgt)
      | _, ITLock _ -> ignore(warn "assignment from non-lock |%a| to lock type" d_tau src)
      | ITAbs _, _
      | _, ITAbs _ -> assert false
      | ITExists ei1, ITExists ei2 -> begin
          sub_tau1 ei1.exist_tau ei2.exist_tau locopt;
          effect_flows ei2.exist_effect ei1.exist_effect;
          CF.phi_flows ei1.exist_phi ei2.exist_phi;
        end
      | _ ->
        ignore(warn "subtyping incompatible types:\n%a\n%a" d_tau src d_tau tgt)
    end

  and sub_tau_list (srclist: tau list)
                   (tgtlist: tau list)
                   (locopt: (rho * rho) option)
                   : unit =
    match (srclist, tgtlist) with
      [], [] -> ()
    | [],{t = ITVoid None}::_
    | ({t = ITVoid None}::_,[]) -> ()
    | {t = ITBuiltin_va_list(r1)}::[], {t = ITBuiltin_va_list(r2)}::[] ->
        unify_rho r1 r2;
    | {t = ITBuiltin_va_list(r1)}::tl1, {t = ITBuiltin_va_list(r2)}::tl2 ->
        unify_rho r1 r2;
        sub_tau_list tl1 tl2 locopt;
    | sl, {t = ITBuiltin_va_list(r)}::s::[]
    | {t = ITBuiltin_va_list(r)}::s::[], sl ->
        ignore(warn "oops %a\n" d_tau s);
        List.iter (fun s -> ignore(E.log "%a\n" d_tau s)) sl;
        assert false (* nothing after "..." *)
    | sl, {t = ITBuiltin_va_list(r)}::[] ->
        let phi_in = make_phi "conflated_in" PhiVar in
        let phi_out = make_phi "conflated_out" PhiVar in
        List.iter (fun s -> conflate_to s r phi_in phi_out TauSet.empty) sl
    | {t = ITBuiltin_va_list(r)}::[], sl ->
        let phi_in = make_phi "conflated_in" PhiVar in
        let phi_out = make_phi "conflated_out" PhiVar in
        List.iter (fun s -> conflate_from r s phi_in phi_out TauSet.empty) sl
    | [],t::_ | (t::_,[]) ->
        ignore(warn "number of args non-equal. first extra arg: %a" d_tau t)
    | s1::tl1,s2::tl2 -> begin
        sub_tau1 s1 s2 locopt;
        sub_tau_list tl1 tl2 locopt;
      end
  in begin
    sub_tau1 src tgt None;
  end

and conflate_from (src: rho)
                  (t: tau)
                  (phi_in: phi)
                  (phi_out: phi)
                  (known: TauSet.t) : unit =
  if TauSet.mem t known then () else
  match t.t with
    (* before was doing nothing; may have been unsafe, since
       conflate_from is sometimes called directly *)
    ITVoid (Some vi) ->
      (* Contents of void*'s does itself get conflated *)
      let v = U.deref vi in
      if !do_void_conflate || !do_void_single then begin
        unify_rho src (getSome v.vinfo_rho);
        CF.unify_phi phi_in (getSome v.vinfo_phi_in);
        CF.unify_phi phi_out (getSome v.vinfo_phi_out);
      end;
  | ITVoid None -> ()
  | ITInt _ -> () (*assert (not b)*)
  | ITFloat -> ()
  | ITTrylockInt _ -> assert false
  | ITPtr(tref,rho) ->
      conflate !tref src phi_in phi_out (TauSet.add t known);
      rho_flows src rho
  | ITFun fi ->
      ignore(warn "function pointer in downcasting.\n");
      List.iter
        (fun (_,argt) ->
          conflate_to argt src phi_in phi_out (TauSet.add t known))
        fi.fd_arg_tau_list;
      if !do_void_conflate || !do_void_single then begin
        CF.phi_flows fi.fd_input_phi phi_in;
        CF.phi_flows phi_out fi.fd_output_phi;
      end;
      conflate_from src fi.fd_output_tau phi_in phi_out (TauSet.add t known)
  | ITComp(ci) ->
      let c = U.deref ci in
      let r = 
        match c.cinfo_from_rho with
          None ->
            defer (fun () ->
              StrHT.iter
                (fun _ (r',t') ->
                  rho_flows src r';
                  conflate_from src t' phi_in phi_out (TauSet.add t known))
                c.cinfo_fields
            );
            src
        | Some r -> r
      in
      (U.deref ci).cinfo_from_rho <- Some r;
      rho_flows src r
  | ITBuiltin_va_list(r) -> unify_rho src r (*XXX: is this ok?*)
  | ITLock(l) ->
      ignore(warn "lock emerges at downcasting: %a\n" d_lock l)
  | ITAbs _ -> assert false
  | ITExists ei ->
      ignore(warn "existential emerges at downcasting");
      conflate_from src ei.exist_tau phi_in phi_out (TauSet.add t known)

and conflate t r phi_in phi_out known =
  conflate_to t r phi_in phi_out known;
  conflate_from r t phi_in phi_out known

and conflate_to (t: tau)
                (tgt: rho)
                (phi_in: phi)
                (phi_out: phi)
                (known: TauSet.t) : unit =
  if TauSet.mem t known then () else
  let known = TauSet.add t known in
  match t.t with
    ITVoid (Some vi) ->
      (* Contents of void*'s does itself get conflated *)
      let v = U.deref vi in
      if !do_void_conflate || !do_void_single then begin
        unify_rho tgt (getSome v.vinfo_rho);
        CF.unify_phi phi_in (getSome v.vinfo_phi_in);
        CF.unify_phi phi_out (getSome v.vinfo_phi_out);
      end
  | ITVoid None
  | ITInt _
  | ITFloat -> ()
  | ITTrylockInt _ -> assert false;
  | ITPtr(tref,rho) ->
      conflate !tref tgt phi_in phi_out known;
      rho_flows rho tgt
  | ITFun fi ->
      ignore(warn "function pointer conflated");
      List.iter
        (fun (_,argt) -> conflate_from tgt argt phi_in phi_out known) fi.fd_arg_tau_list;
      if !do_void_conflate || !do_void_single then begin
        CF.phi_flows phi_in fi.fd_input_phi;
        CF.phi_flows fi.fd_output_phi phi_out;
      end;
      conflate_to fi.fd_output_tau tgt phi_in phi_out known
  | ITComp(ci) ->
      let c = U.deref ci in
      let r = 
        match c.cinfo_to_rho with
          None ->
            defer (fun () ->
              StrHT.iter
                (fun _ (r',t') ->
                  rho_flows r' tgt;
                  conflate_to t' tgt phi_in phi_out (TauSet.add t known))
                c.cinfo_fields);
            tgt
        | Some r -> r
      in
      c.cinfo_to_rho <- Some r;
      rho_flows r tgt
  | ITBuiltin_va_list(r) -> unify_rho tgt r (*XXX: is this ok?*)
  | ITLock(l) -> ignore(warn "lock is conflated! (%a)" d_lock l)
  | ITAbs _ -> assert false
  | ITExists ei ->
      ignore(warn "Existential labels conflated!!!");
      conflate_to ei.exist_tau tgt phi_in phi_out (TauSet.add t known)

and unify_types (s1: tau) (s2: tau) : unit =
  if s1 == s2 then ()
  else
    match s1.t, s2.t with
      ITAbs si1, ITAbs si2 ->
        sub_tau !si1 !si2;
        sub_tau !si2 !si1;
    | _ ->
        sub_tau s1 s2;
        sub_tau s2 s1;

(* this is used only with --single-void
 *)
and conflate_vinfo (v: voiddata) : unit = begin
  match v.vinfo_types with
    ConflatedRho -> ()
  | ListTypes [(_,t)] -> begin
      let r = getSome v.vinfo_rho in
      let phi_in = getSome v.vinfo_phi_in in
      let phi_out = getSome v.vinfo_phi_out in
      conflate t r phi_in phi_out TauSet.empty;
      v.vinfo_types <- ConflatedRho
    end
  | ListTypes [] ->
      v.vinfo_types <- ConflatedRho
  | _ -> assert false
end

and add_type_to_vinfo (s: tau) (v: voiddata) : unit = begin
  if !debug_void then ignore(E.log "add_type_to_vinfo: add %a to #%d\n" d_tau s v.vinfo_id);
  match v.vinfo_types with
    ConflatedRho ->
      if !debug_void then ignore(E.log "conflated\n");
      let r = getSome v.vinfo_rho in
      let phi_in = getSome v.vinfo_phi_in in
      let phi_out = getSome v.vinfo_phi_out in
      conflate s r phi_in phi_out TauSet.empty;
  | ListTypes tl -> begin
      try
        let t = List.assoc s.ts tl in
        unify_types s t
      with Not_found -> begin
        if !debug_void then ignore(E.log "conflating list\n");
        if !do_void_single && (tl <> []) then begin
          conflate_vinfo v;
          add_type_to_vinfo s v;
        end else begin
          v.vinfo_types <- ListTypes ((s.ts,s)::tl)
        end
      end
    end
end

and get_cinfo_field (fi: fieldinfo) (ci: cinfo)
                    (uniq: bool) (* if this is true, any allocated fields will
                                  * add their & rhos in thread_local_rhos *)
                    (known: (tau_sig * tau) list)
                    (name: LN.label_name)
                    : (rho * tau) =
  let compdata = U.deref ci in
  assert (compdata.compinfo == fi.fcomp);
  try
    StrHT.find compdata.cinfo_fields fi.fname
  with Not_found -> begin
    let storeloc = !Cil.currentLoc in
    Cil.currentLoc := compdata.cinfo_loc;
    let r = make_rho (LN.Field (name, fi.fname)) false in
    (* BEGIN OBSOLETE *)
    if uniq then thread_local_rhos := RhoSet.add r !thread_local_rhos;
    (* END OBSOLETE *)
    let s =
      annotate fi.ftype (if !do_compact_structs then known else [])
        (LN.Field (name, fi.fname)) in
    (* why was this commented out?  seems necessary *)
    (*XXX these phis are not completely sound *)
    if !debug_void then ignore(E.log "get_cinfo_field: add %s:%a to #%d\n" fi.fname d_tau s compdata.cinfo_id);
    let phi_in = make_phi "conflated_in" PhiVar in
    let phi_out = make_phi "conflated_out" PhiVar in
    begin
      match compdata.cinfo_to_rho with
        None -> ()
      | Some r' -> conflate_to s r' phi_in phi_out TauSet.empty; rho_flows r r'
    end;
    begin
      match compdata.cinfo_from_rho with
        None -> ()
      | Some r' -> conflate_from r' s phi_in phi_out TauSet.empty; rho_flows r' r
    end;
    StrHT.add compdata.cinfo_fields fi.fname (r,s);
    Cil.currentLoc := storeloc;
    (r,s)
  end
  

and instantiate (tabs: tau)
                (tinst: tau)
                (polarity: bool)
                (i: instantiation)
                : unit =
  let rec instantiate1 (tabs: tau)
                       (tinst: tau)
                       (polarity: bool)
                       (i: instantiation)
                       : unit =
    if InstEdgeTbl.mem inst_edges (tabs,tinst,i,polarity) then ()
    else begin
      InstEdgeTbl.add inst_edges (tabs,tinst,i,polarity) ();
      match (tabs.t,tinst.t) with
      | ITVoid None, ITVoid None -> ()
      | ITFloat, ITFloat -> ()
      | ITPtr(tref1, r1), ITPtr(tref2, r2) -> begin
          match (!tref1).t, (!tref2).t with
            ITVoid(Some vi1), ITVoid(Some vi2) ->
              inst_rho r1 r2 polarity i;
              let v =
                try VinfoInstHash.find inst_vinfo_hash (vi1,i) with
                Not_found -> vi2
              in
              U.unify unify_vinfo (v,vi2);
              inst_vinfo vi1 v i
          | _ ->
            inst_rho r1 r2 polarity i;
            (* both plus and minus for pointed type: *)
            instantiate1 !tref1 !tref2 true i;
            instantiate1 !tref1 !tref2 false i;
        end
      | ITInt b1, ITInt b2 -> assert (not b1); assert (not b2); ()
      | ITInt _, ITFloat -> ()
      | ITFloat, ITInt false -> ()

      | ITVoid (Some _), _
      | _, ITVoid (Some _) -> assert false

      | ITTrylockInt _, _
      | _, ITTrylockInt _ -> assert false
      | ITFun fi1, ITFun fi2 ->
          begin
            try
              List.iter2
                (fun (n1,t1) (n2, t2) ->
                  (*assert (n1 = n2);*)
                  instantiate1 t1 t2 (not polarity) i)
                fi1.fd_arg_tau_list
                fi2.fd_arg_tau_list;
            with Invalid_argument _ ->
              ignore(E.warn "instantiating functions with different \
                             numbers of arguments:\n %a\n %a"
                             d_tau tabs d_tau tinst);
          end;
          CF.inst_phi fi1.fd_input_phi fi2.fd_input_phi (not polarity) i;
          CF.inst_phi fi1.fd_output_phi fi2.fd_output_phi polarity i;
          inst_effect fi1.fd_input_effect fi2.fd_input_effect polarity i;
          inst_effect fi1.fd_output_effect fi2.fd_output_effect (not polarity) i;
          inst_lock_effect fi1.fd_lock_effect fi2.fd_lock_effect polarity i;
          S.epsilon_inst fi1.fd_epsilon i fi2.fd_epsilon;
          instantiate1 fi1.fd_output_tau fi2.fd_output_tau polarity i;
          (*inst_chi fi1.fd_chi fi2.fd_chi (not polarity) i;*)
          inst_chi fi1.fd_chi fi2.fd_chi polarity i;
      | ITComp(ci1), ITComp(ci2) ->
          let c =
            try CinfoInstHash.find inst_cinfo_hash (ci1,i) with
            Not_found -> ci2
          in
          U.unify unify_cinfo (c,ci2);
          inst_cinfo ci1 c i;
      | ITBuiltin_va_list(r1), ITBuiltin_va_list(r2) ->
          inst_rho r1 r2 true i;
          inst_rho r1 r2 false i
      | ITLock(l1), ITLock(l2) ->
          inst_lock l1 l2 i
      | ITAbs _, _
      | _, ITAbs _ -> assert false
      | ITExists ei1, ITExists ei2 ->
          instantiate1 ei1.exist_tau ei2.exist_tau polarity i;
          inst_effect ei1.exist_effect ei2.exist_effect (not polarity) i;
          inst_effect ei1.exist_effect ei1.exist_effect true i;
          inst_effect ei1.exist_effect ei1.exist_effect false i;
          CF.inst_phi ei1.exist_phi ei2.exist_phi polarity i;
          CF.inst_phi ei1.exist_phi ei1.exist_phi true i;
          CF.inst_phi ei1.exist_phi ei1.exist_phi false i;
          RhoSet.iter
            (fun x -> inst_rho x x true i; inst_rho x x false i)
            ei1.exist_rhoset;
          LockSet.iter
            (fun x -> inst_lock x x i)
            ei1.exist_lockset;
          EffectSet.iter
            (fun x -> inst_effect x x true i; inst_effect x x false i)
            ei1.exist_effectset;
      | (_,_) ->
          ignore(E.log "%a\n%a\n" d_tau tabs d_tau tinst);
          raise (TypingBug "instantiating types with different structures")
    end
  in begin
    instantiate1 tabs tinst polarity i;
  end
(****************************************************************************)
(* helper function that decides the return type of binary operators *)
and type_binop (b : binop)
               (s1: tau)
               (s2: tau)
               : tau =
  match b with
    PlusPI
  | PlusA
  | MinusPI | IndexPI -> s1
  | MinusPP
  | MinusA | Mult | Div | Mod 
  | Shiftlt | Shiftrt | BAnd | BXor | BOr -> integer_tau
  | Lt | Gt | Le | Ge | LAnd | LOr -> integer_tau
  | Eq -> (
      match s1.t,s2.t with
        ITTrylockInt(l,b), ITInt true -> make_tau (ITTrylockInt(l, b)) STInt
      | _ -> integer_tau
    )
  | Ne -> (
      match s1.t,s2.t with
        ITTrylockInt(l,b), ITInt true -> make_tau (ITTrylockInt(l, not b)) STInt
      | _ -> integer_tau
    )

(*****************************************************************************)
(* JUDGEMENTS *)
(*****************************************************************************)

(* judgements have
 * inputs:
 *  - the structure that is being typed.
 *  - input environment
 *  - input phi
 *  - input effect 
 * outputs:
 *  - tau (returned type)
 *  - output rho (for stuff whose address might be taken)
 *  - output environment
 *  - output phi
 *  - output effect 
 * when a type-rule doesn't need/return all of them, the corresponding function
 * might not have them in its signature.
 *)

and type_const (c: constant)
               (input_env: env)
               (input_phi: phi)
               (input_effect: effect)
               : ((tau * uniq) * env * phi * effect) =
  match c with
  | CChr(i) ->
      let rt = make_tau (ITInt(i = '\000')) STInt in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CInt64(i,_,_) ->
      let rt = make_tau (ITInt(i = Int64.zero)) STInt in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CStr(_) | CWStr(_) ->
      let r = make_rho (LN.Const "const-string") true in
      let rt = make_tau (ITPtr(ref integer_tau, r)) (STPtr STInt) in
      ((rt,NotUnq), input_env, input_phi, input_effect)
  | CReal(_,_,_) ->
      let t = make_tau ITFloat STFloat in
      ((t,NotUnq), input_env, input_phi, input_effect)

and type_var (var: varinfo)     (* name of variable typed *)
             (input_env: env)   (* input environment *)
             (input_phi: phi)   (* pipe the flow-sensitive environment through
                                 * --not used, is there just in case *)
             (input_effect: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  let (s, r) =
    try
      env_lookup var.vname input_env
    with Not_found -> raise (TypingBug ("undefined variable "^var.vname))
  in
  let (out_rho, out_env) = (r, input_env) in
  let unq = 
    if !do_uniq && Uniq.is_unique var.vname !current_uniqueness then
      UnqVar 
    else NotUnq
  in
  if !debug then ignore(E.log "var |%s| is %s\n" var.vname (uniq2str unq));
  let out_tau = (
    match s.t with
    | ITAbs tref -> (
        match (!tref).t with
          ITFun fi ->
            if !do_context_insensitive then (
              !tref
            ) else (
              let i = make_instantiation false var.vname in
              let tinst = reannotate !tref [] (LN.Const var.vname) in
              if !debug then
                ignore(E.log "instantiating %s on %a\n   %a\n   %a\n"
                             var.vname
                             d_instantiation i
                             d_tau !tref
                             d_tau tinst);
              instantiate !tref tinst true i;
              tinst
            )
        | _ -> assert false;
      )
    | _ -> s
  ) in
  ((out_tau, out_rho, unq), out_env, input_phi, input_effect)

and type_lval (h,o : lhost * offset) (* lvals are host * offset pairs *)
             (input_env: env)
             (input_phi: phi)
             (input_eff: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  match (h,o) with
    (Mem (CastE(t,_)), Field(_, _)) ->
      let temp = !do_ignore_casts in
      do_ignore_casts := false;
      let ((host_type, host_rho, host_uniq), host_env, host_phi, host_effect) =
        type_host h input_env input_phi input_eff in
      let name = LN.Const(string_of_doc (d_lval () (h, NoOffset))) in
      let result = type_offset o (host_type, host_rho, host_uniq) name host_env host_phi host_effect in
      do_ignore_casts := temp;
      result
   | _ ->
      let ((host_type, host_rho, host_uniq), host_env, host_phi, host_effect) =
      type_host h input_env input_phi input_eff in
      let name = LN.Const (string_of_doc (d_lval () (h, NoOffset))) in
      type_offset o (host_type, host_rho, host_uniq) name host_env host_phi host_effect

and type_offset
               (o: offset)
               (host_type, host_rho, host_uniq : tau * rho * uniq)
               (name: LN.label_name)
               (input_env: env)
               (input_phi: phi)
               (input_effect: effect)
               : ((tau * rho * uniq) * env * phi * effect) =
  match o with
    NoOffset -> ((host_type, host_rho, host_uniq), 
                 input_env, input_phi, input_effect)
  | Field(fi, o1) -> begin
      read_rho host_rho input_phi input_effect host_uniq;
      match host_type.t with
        ITComp(c) -> begin
            (* BEGIN OBSOLETE *)
            let uniq = RhoSet.mem host_rho !thread_local_rhos in
            (* END OBSOLETE *)
            let r,s = get_cinfo_field fi c uniq [(host_type.ts, host_type)] name in
            (* field offsets inherit the host's uniqueness *)
            type_offset o1 (s,r,host_uniq) (LN.Field (name, fi.fname)) input_env input_phi input_effect
        end
      | _ ->
        ignore(warn "trying to access field %s of non-struct type:\n  %a\n"
                     fi.fname d_tau host_type);
        raise (TypingBug "trying to access a field of non-struct type\n")
(*        ((ITInt false, unknown_rho), input_env, p1, input_effect) *)
    end
  | Index(e, o1) ->
      read_rho host_rho input_phi input_effect host_uniq;
      let ((exp_type,exp_uniq), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect in
      match exp_type.t with
        ITInt _ | ITFloat -> begin
          match host_type.t with
            | ITPtr(s2, r2) -> begin
              read_rho r2 exp_phi exp_effect exp_uniq;
              type_offset o1 (!s2,r2,NotUnq) (LN.Deref name) exp_env exp_phi exp_effect
            end
          | _ ->
              raise (TypingBug "cannot index something that isn't an array")
        end
      | _ ->
          ignore(E.log "%a\n" d_tau exp_type);
          raise (TypingBug "cannot index array using non-int!")

and type_host (h: lhost)
             (input_env: env)
             (input_phi: phi)
             (input_effect: effect)
             : ((tau * rho * uniq) * env * phi * effect) =
  match h with
    Var(v) ->
      type_var v input_env input_phi input_effect
  | Mem(e) ->
      let ((exp_type,exp_uniq), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect
      in begin
        match exp_type.t with
          ITPtr(s1, r) ->
            (* MWH: this is redundant; either the e under the
               Mem() is an lval and thus we got in in type_exp
               or else it's a constant and we don't care. *)
            read_rho r exp_phi exp_effect exp_uniq;
            ((!s1, r, uniq_deref exp_uniq), exp_env, exp_phi, exp_effect)
        | _ ->
          ignore(E.log "%a: dereferencing non-pointer: %a : %a\n" Cil.d_loc !Cil.currentLoc d_exp e d_tau exp_type);
          ((integer_tau, unknown_rho, exp_uniq), exp_env, exp_phi, exp_effect)
          (*raise (TypingBug (string_of_doc d))*)
      end

and type_ref (lv: lval)
            (input_env: env)
            (input_phi: phi)
            (input_effect: effect)
            : ((tau * uniq) * env * phi * effect) =
  let ((s, r, _), lval_env, lval_phi, lval_effect) =
    type_lval lv input_env input_phi input_effect (*true*)
  in
  let rt = make_tau (ITPtr(ref s, r)) (STPtr s.ts) in
  ((rt,NotUnq), lval_env, lval_phi, lval_effect)

and type_exp (e: exp)
             (input_env: env)
             (input_phi: phi)
             (input_effect: effect) 
             : ((tau * uniq) * env * phi * effect) =
  match e with
    Const(c) ->
      type_const c input_env input_phi input_effect
  | Lval(lv) -> begin
      let ((s, r, u), lval_env, lval_phi, lval_effect) =
        type_lval lv input_env input_phi input_effect (*false*)
      in
      if !debug then ignore(E.log "lval |%a| is %s\n" d_lval lv (uniq2str u));
      read_rho r lval_phi input_effect u;
      ((s,u), lval_env, lval_phi, lval_effect)
    end
  | SizeOf(_) | SizeOfE(_) | SizeOfStr(_)
  | AlignOf(_) | AlignOfE(_) ->
      ((integer_tau,NotUnq), input_env, input_phi, input_effect)
  | UnOp(_,e1,_) ->
      let (_, exp_env, exp_phi, exp_effect) =
        type_exp e1 input_env input_phi input_effect
      in ((integer_tau,NotUnq), exp_env, exp_phi, exp_effect)
  | BinOp(bop, e1, e2, _) ->
      let ((s1,u1), exp_env1, exp_phi1, exp_effect1) =
        type_exp e1 input_env input_phi input_effect in
      let ((s2,u2), exp_env2, exp_phi2, exp_effect2) =
        type_exp e2 exp_env1 exp_phi1 exp_effect1 in
      let u = match bop with 
        PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP -> u1
      | _ -> NotUnq in
      (((type_binop bop s1 s2),u), exp_env2, exp_phi2, exp_effect2)
  | CastE(t,e1) ->
      if !do_ignore_casts then
        type_exp e1 input_env input_phi input_effect
      else
        let ((s1,u1), exp_env, exp_phi, exp_eff) =
          type_exp e1 input_env input_phi input_effect in
        let s2 = annotate t [] (LN.Const "cast") in
        sub_tau s1 s2;
        ((s2,u1), exp_env, exp_phi, exp_eff)
  | AddrOf(lv) ->
      type_ref lv input_env input_phi input_effect
  | StartOf(lv) ->
      let ((s,_,u), lval_env, lval_phi, lval_effect) =
        type_lval lv input_env input_phi input_effect (*false*) in
      begin
        match s.t with
        ITPtr(_, _) -> ((s,u), lval_env, lval_phi, lval_effect)
        | _ -> raise (TypingBug "StartOf doesn't return pointer?!" )
      end

(*****************************************************************************)
and compute_quantified_labels (c: cinfo) (el: exp list) : labelsets =
  let cc = (U.deref c).compinfo in
  let t = make_tau (ITComp(c)) (STComp(cc.cstruct,cc.cname)) in
  let env = env_add_var (fresh_env ()) cc.cname (t, unknown_rho) in
  List.fold_left
    (fun ls e ->
      let (tt,_),_,_,_ = type_exp e env CF.empty_phi empty_effect in
      let l = get_top_label tt e in
      add_label_to_labelsets l ls)
    empty_labelsets
    el

and fillExist (te:tau) (t: tau) : unit = begin
  let e = 
    match te.t with
    | ITExists(e) -> e
    | _ -> raise (TypingBug "impossible");
  in
  assert(not e.exist_initialized);
  let c =
    match t.t with
      ITComp c -> c
    | _ -> raise (TypingBug "fillExist should only be called for structs")
  in
  let el = Hashtbl.find_all quantified_map ((U.deref c).compinfo.cname) in
  e.exist_tau <- t;
  e.exist_abs <- el;
  let rs, ls, es, ps = compute_quantified_labels c el in
  e.exist_rhoset <- rs;
  e.exist_lockset <- ls;
  e.exist_effectset <- es;
  e.exist_initialized <- true;
end

and type_exp_list (el: exp list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                : ((tau * uniq) list * env * phi * effect) =
  match el with
    [] -> ([], input_env, input_phi, input_effect)
  | h::tl ->
      let (s, exp_env, exp_phi, exp_effect) =
        type_exp h input_env input_phi input_effect in
      let (ls, out_env, out_phi, out_effect) =
        type_exp_list tl exp_env exp_phi exp_effect in
      (s::ls, out_env, out_phi, out_effect)

and type_init (i: init)
              (input_env: env)
              (name: LN.label_name)
              : tau * env =
  match i with
  | SingleInit(e) ->
      let ((s1,_), exp_env, exp_phi, exp_effect) =
        type_exp e input_env CF.empty_phi empty_effect in
      if exp_phi != CF.empty_phi || exp_effect != empty_effect
      then raise (TypingBug "initializer with effect and/or phi")
      else (s1, exp_env)
  | CompoundInit(t, l) ->
      let s = annotate t [] name in
      allocate s;
      let out_env = 
        match s.t with
        | ITLock(_) ->
            let lt = make_tau (ITLock(make_lock name true)) STLock in
            sub_tau lt s;
            input_env
        | ITComp(ci) -> List.fold_left (type_offcinit ci name) input_env l
        | ITPtr(tref, _) -> List.fold_left (type_offainit !tref name) input_env l
        | _ ->
            raise (TypingBug "compound initializer with non-compound type")
      in (s, out_env)

and type_offainit (array_type: tau)
                  (name: LN.label_name)
                  (input_env: env)
                  (o, i: offset * init)
                  : env =
  let (si, out_env) = type_init i input_env (LN.Deref name) in
  match o with
    Index(_, NoOffset) ->
      sub_tau si array_type;
      out_env
  | _ -> raise (TypingBug "initializer of the wrong type")

and type_offcinit (c: cinfo)
                  (name: LN.label_name)
                  (input_env: env)
                  (o, i: offset * init)
                  : env =
  match o with
  | Field(f, NoOffset) ->
      let (si, out_env) = type_init i input_env (LN.Field(name,f.fname)) in
      let _,s = get_cinfo_field f c false [] name in
      sub_tau si s;
      out_env
  | _ -> raise (TypingBug "initializer of the wrong type")

(*****************************************************************************)
let handle_newlock (el: exp list)
                   (_: lval option)
                   (args: (tau*uniq) list)
                   (input_env: env)
                   (input_phi: phi)
                   (input_effect: effect)
                   (input_lock_effect: lock_effect)
                   : gamma * S.epsilon =
  let name =
    match el with
      x::_ -> LN.Const(string_of_lock_exp x)
    | _ -> raise (TypingBug "calling newlock without arguments")
  in
  let nl = make_lock name true in
  match args with
    [] -> raise (TypingBug "calling newlock without arguments")
  | (h,_)::_ ->
      let lt = make_tau (ITLock nl) STLock in
      let pt = make_tau (ITPtr(ref lt, make_rho (LN.AddrOf name) false)) (STPtr STLock) in
      sub_tau pt h;
      let out_phi = make_phi "newlock" (PhiNewlock nl) in
      CF.phi_flows input_phi out_phi;
      (input_env, out_phi, input_effect), S.singleton nl

let handle_trylock (el: exp list)
                   (lvo: lval option)
                   (args: (tau*uniq) list)
                   (input_env: env)
                   (input_phi: phi)
                   (input_effect: effect)
                   (input_lock_effect: lock_effect)
                   : gamma * S.epsilon =
  let name =
    match el with
      x::_ -> LN.Const(string_of_lock_exp x)
    | _ -> raise (TypingBug "calling trylock without arguments")
  in
  match args with
    ({t = ITPtr(tref, _)},_)::_ -> begin
      let l =
        match (!tref).t with
          ITLock(l) -> l
        | _ -> make_lock name false
      in
      let locktype = make_tau (ITLock(l)) STLock in
      sub_tau !tref locktype;
      add_to_lock_effect l input_lock_effect;
      begin
        match lvo with
          Some((Var(v),_)) ->
            let t = make_tau (ITTrylockInt(l,true)) STInt in
            let (oldt, r) = env_lookup v.vname input_env in
            let out_env = env_add_var input_env v.vname (t, r) in
            if !debug then
              ignore(E.log "trylock: replacing %a with %a"
                d_tau oldt d_tau t);
            (out_env, input_phi, input_effect), S.empty_epsilon
          | None ->
              raise (TypingBug "ignoring trylock result")
          | _ ->
              ignore(warn "lost trylock result");
              (input_env, input_phi, input_effect), S.empty_epsilon
      end
    end
  | _ ->
      raise (TypingBug "calling trylock without lock arguments")

let handle_acquire (el: exp list)
                   (lvo: lval option)
                   (args: (tau*uniq) list)
                   (input_env: env)
                   (input_phi: phi)
                   (input_effect: effect)
                   (input_lock_effect: lock_effect)
                   : gamma * S.epsilon =
(*  match lvo with
    Some((Var(v),_)) ->
      handle_trylock el lvo args input_env input_phi input_effect
  |_ -> begin*)
  let name =
    match el with
      x::_ -> LN.Const(string_of_lock_exp x)
    | _ -> raise (TypingBug "calling acquire without arguments")
  in
  match args with
    ({t = ITPtr(tref, _)},_)::_ -> begin
      match (!tref).t with
        ITLock(l) ->
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "acquire" (PhiAcquire l) in
          CF.phi_flows input_phi out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
      | _ ->
          let l = make_lock name false in
          let locktype = make_tau (ITLock(l)) STLock in
          sub_tau !tref locktype;
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "acquire" (PhiAcquire l) in
          CF.phi_flows input_phi out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
    end
  | _ ->
      ignore(E.log "impossible: %a\n" d_taulist (fst (List.split args)));
      raise (TypingBug "calling lock without lock arguments")
  (*end*)

let handle_release (el: exp list)
                   (_: lval option)
                   (args: (tau*uniq) list)
                   (input_env: env)
                   (input_phi: phi)
                   (input_effect: effect)
                   (input_lock_effect: lock_effect)
                   : gamma * S.epsilon =
  let name =
    match el with
      x::_ -> LN.Const(string_of_lock_exp x)
    | _ -> raise (TypingBug "calling release without arguments")
  in
  match args with
    ({t = ITPtr(tref, _)},_)::_ -> begin
      match (!tref).t with
        ITLock(l) ->
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "release" (PhiRelease l) in
          CF.phi_flows input_phi out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
      | _ ->
          let l = make_lock name false in
          let locktype = make_tau (ITLock(l)) STLock in
          sub_tau !tref locktype;
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "release" (PhiRelease l) in
          CF.phi_flows input_phi out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
    end
  | _ -> raise (TypingBug "calling unlock without lock arguments")
                   
let handle_destroylock (el: exp list)
                       (_: lval option)
                       (args: (tau*uniq) list)
                       (input_env: env)
                       (input_phi: phi)
                       (input_effect: effect)
                       (input_lock_effect: lock_effect)
                       : gamma * S.epsilon =
  let name =
    match el with
      x::_ -> LN.Const(string_of_lock_exp x)
    | _ -> raise (TypingBug "calling destroylock without arguments")
  in
  match args with
    ({t = ITPtr(tref, _)},_)::_ -> begin
      match (!tref).t with
        ITLock(l) ->
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "delete" (PhiDelete l) in
          CF.phi_flows input_phi out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
      | _ ->
          let l = make_lock name false in
          let locktype = make_tau (ITLock(l)) STLock in
          sub_tau !tref locktype;
          add_to_lock_effect l input_lock_effect;
          let out_phi = make_phi "delete" (PhiDelete l) in
          CF.phi_flows input_phi  out_phi;
          (input_env, out_phi, input_effect), S.empty_epsilon
    end
  | _ -> raise (TypingBug "calling lock without lock arguments")

let handle_exit (_: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                (input_lock_effect: lock_effect)
                : gamma * S.epsilon =
  (input_env, CF.empty_phi, LF.make_effect "exit-effect" false), S.empty_epsilon

let handle_fork (_: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                (input_lock_effect: lock_effect)
                : gamma * S.epsilon =
  match args with
    ({t =ITPtr(_,r1)},u1)::(t2,u2)::({t= ITPtr(tref, b)},u3)::(arg,_)::[] ->
      let fi = 
        match (!tref).t with
          ITFun fi -> fi
        | _ -> assert false;
      in
      let _ =
        match fi.fd_arg_tau_list with
          (_,a)::_ -> sub_tau arg a
        | [] -> ()
      in
      read_rho r1 input_phi input_effect u1;
      read_rho b input_phi input_effect u3;
      begin
        match t2.t with
          ITPtr(_,r2) ->
            read_rho r2 input_phi input_effect u2
        | ITInt true -> ()
        | _ -> raise (TypingBug "bad fork")
      end;
      (* both continuation and forked thread effects flow to input effect *)
      let eff_after = LF.make_effect "fork-effect" true in
      let eff_forked = LF.make_effect "forked-effect" true in
      effect_flows fi.fd_input_effect eff_forked;
      effect_flows eff_after input_effect;
      chi_flows fi.fd_chi !current_chi;

      (* starting phi of the new thread and phi immediately after the fork
         both follow after phi_before *)
      let phi_before = input_phi in
      let phi_forked = make_phi "FORK" PhiForked in
      let phi_after = make_phi "afterfork" PhiVar in
      CF.starting_phis := phi_forked::!CF.starting_phis;
      CF.phi_flows phi_forked fi.fd_input_phi;
      CF.phi_flows phi_before phi_after;
      CF.phi_flows phi_before phi_forked;      

      (*let live = !current_liveness in*)
      add_fork input_effect eff_after eff_forked
        phi_before phi_after phi_forked 
        (function () -> let (_,rs) = find_free_vars input_env in rs);

      (* log *)
      if !debug then
        ignore(E.log "fork: %a\n -> %a\n -> %a\n"
          CF.d_phi phi_before
          CF.d_phi phi_after
          CF.d_phi phi_forked);
      (input_env, phi_after, eff_after), fi.fd_epsilon
  | _ -> raise (TypingBug "calling fork with bad arguments")

let handle_memset (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  (input_lock_effect: lock_effect)
                  : gamma * S.epsilon =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::_ ->
      write_rho r1 input_phi input_effect u1;
      (input_env, input_phi, input_effect), S.empty_epsilon
  | _ -> raise (TypingBug "calling memset with bad arguments")

let handle_memcpy (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  (input_lock_effect: lock_effect)
                  : gamma * S.epsilon =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::({t=(ITPtr(s2,r2))},u2)::_ ->
      sub_tau !s2 !s1;
      write_rho r1 input_phi input_effect u1;
      read_rho r2 input_phi input_effect u2;
      visit_concrete_tau (fun r -> write_rho r input_phi input_effect u1) !s1;
      visit_concrete_tau (fun r -> read_rho r input_phi input_effect u2) !s2;
      (input_env, input_phi, input_effect), S.empty_epsilon
  | _ -> raise (TypingBug "calling memcpy with bad arguments")

let handle_va_start (el: exp list)
                    (_: lval option)
                    (args: (tau*uniq) list)
                    (input_env: env)
                    (input_phi: phi)
                    (input_effect: effect)
                    (input_lock_effect: lock_effect)
                    : gamma * S.epsilon =
  match el with
    _::Lval(Var(vi), NoOffset)::_
  | _::CastE(_,Lval(Var(vi), NoOffset))::_ ->
      let ((t,r,u),env,phi,eff) =
        type_var vi input_env input_phi input_effect in
      read_rho r phi eff u;
      (env, phi, eff), S.empty_epsilon
  | _ -> raise (TypingBug "calling va_start with bad arguments")

let handle_va_arg (_: exp list)
                  (lvo: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  (input_lock_effect: lock_effect)
                  : gamma * S.epsilon =
  match lvo with
    None -> raise (TypingBug "calling va_arg without assigning the result")
  | Some(lv) -> begin
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect in
      match args with
        ({t = ITBuiltin_va_list(r)},_)::_ ->
          let phi_in = make_phi "conflated_in" PhiVar in
          let phi_out = make_phi "conflated_out" PhiVar in
          conflate_from r lv_type phi_in phi_out TauSet.empty;
          (lv_env, lv_phi, lv_effect), S.empty_epsilon
      | _ -> raise (TypingBug "calling va_arg without a va_list arg")
    end

let handle_strcmp (_: exp list)
                  (_: lval option)
                  (args: (tau*uniq) list)
                  (input_env: env)
                  (input_phi: phi)
                  (input_effect: effect)
                  (input_lock_effect: lock_effect)
                  : gamma * S.epsilon =
  match args with
    ({t=(ITPtr(s1,r1))},u1)::({t=(ITPtr(s2,r2))},u2)::_ ->
      read_rho r1 input_phi input_effect u1;
      read_rho r2 input_phi input_effect u2;
      (input_env, input_phi, input_effect), S.empty_epsilon
  | _ -> raise (TypingBug "calling strcmp with bad arguments")

let handle_start_unpack (el: exp list)
                        (lv: lval option)
                        (args: (tau*uniq) list)
                        (input_env: env)
                        (input_phi: phi)
                        (input_effect: effect)
                        (input_lock_effect: lock_effect)
                        : gamma * S.epsilon =
  if !do_existentials then begin
    let err = (TypingBug "start_unpack takes a single (local) variable, pointer to existential struct") in
    match el with
      [Lval(Var(vi), NoOffset)] -> begin
        let (tabsptr, var_rho) = env_lookup vi.vname input_env in
        if var_rho <> unknown_rho then
          ignore(warn "unpacking variable whose address is taken: unsound!!\n");
        match tabsptr.t with
          ITPtr(tref, ptr_rho) ->
            let ei =
              match !tref.t with
                ITExists ei -> ei
              | _ -> raise err
            in
            begin
              match ei.exist_tau.t with
                ITComp ci -> ()
              | _ -> raise err;
            end;
            let newt =
              make_tau (ITPtr(ref ei.exist_tau, ptr_rho))
                       (STPtr ei.exist_tau.ts)
            in
            let unpack_phi = make_phi "unpack" (PhiPack ei.exist_phi) in
            CF.phi_flows ei.exist_phi unpack_phi;
            CF.phi_flows unpack_phi input_phi;
            let eff = LF.make_effect "unpack" true in
            effect_flows eff ei.exist_effect;
            effect_flows eff input_effect;
            let e2 = env_add_unpack_ei input_env vi.vname ei (newt, var_rho) in
            (e2, input_phi, eff), S.empty_epsilon
        | _ -> raise err
      end
    | _ -> raise err
  end else (input_env, input_phi, input_effect), S.empty_epsilon

let handle_end_unpack (el: exp list)
                      (lv: lval option)
                      (args: (tau*uniq) list)
                      (input_env: env)
                      (input_phi: phi)
                      (input_effect: effect)
                      (input_lock_effect: lock_effect)
                      : gamma * S.epsilon =
  if !do_existentials then begin
    let err = (TypingBug "end_unpack takes a single (local) variable, pointer to an unpacked existential struct") in
    match el with
      [Lval(Var(vi), NoOffset)] -> begin
        let (tabsptr, var_rho) = env_lookup vi.vname input_env in
        if var_rho <> unknown_rho then
          ignore(warn "end_unpack given variable whose address is taken: unsound!!\n");
        match tabsptr.t with
          ITPtr(tref, ptr_rho) ->
            begin
              match !tref.t with
                ITComp ci -> ()
              | _ -> raise err
            end;
            let ei = Strmap.find vi.vname input_env.unpacked_map in
            let ets = (STExists(ei.exist_tau.ts)) in
            let et = make_tau (ITExists ei) ets in
            let newt = make_tau (ITPtr(ref et, var_rho)) (STPtr ets) in
            let e1 = env_add_var input_env vi.vname (newt, var_rho) in
            let e2 = env_del_unpack_ei e1 vi.vname in
            (e2, input_phi, input_effect), S.empty_epsilon
        | _ -> raise err
      end
    | _ -> raise err
  end else (input_env, input_phi, input_effect), S.empty_epsilon

(* pack(x) happens here.  x has to be a variable (not path),
 * pointer to a struct.  An ITExists type is created and returned.
 *)
let handle_pack (el: exp list) (* arguments to pack, should be singleton list *)
                (lvo: lval option)
                (tl: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                (input_lock_effect: lock_effect)
                : gamma * S.epsilon =
  if !do_existentials then begin
    let err = (TypingBug "pack should a single argument, pointer to struct and return into an existential") in
    match tl with
      ({t=ITPtr(tref, ptr_rho)},_)::[] -> begin
        begin
        match !tref.t with
          ITComp ci -> ()
          | _ -> ignore(E.log "pack argument is: %a\n" d_tau !tref);raise err
        end;
        let i = make_instantiation true "pack" in
        let tinst = !tref in
        let name =
          match el with
            [Lval(Var v, NoOffset)] -> LN.Const v.vname
          | _ -> raise err
        in
        let tabs = reannotate tinst [] name in
        instantiate tabs tinst false i;
        let et = mkEmptyExist tabs.ts in
        fillExist et tabs;
        let pack_phi = make_phi "pack" (PhiPack input_phi)  in
        CF.phi_flows input_phi pack_phi;
        (match et.t with
          ITExists ei ->
            CF.inst_phi ei.exist_phi pack_phi false i;
            inst_effect ei.exist_effect input_effect true i;
        | _ -> assert false);
        let texistsptr = make_tau (ITPtr(ref et, ptr_rho)) (STPtr et.ts) in
        match lvo with
          Some lv ->
            let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
              type_lval lv input_env input_phi input_effect in
            sub_tau texistsptr lv_type;
            (lv_env, lv_phi, lv_effect), S.empty_epsilon
        | None -> raise err
      end
    | _ -> raise err
  end else begin
    match tl with
      [(t,_)] -> begin
        let (t',_,_), env, phi, eff =
          match lvo with
            None -> raise (TypingBug "pack result not assigned");
          | Some lv ->
              type_lval lv input_env input_phi input_effect
        in
        sub_tau t t';
        (*ignore(E.log "%a to %a\n" d_tau t d_tau t');*)
        (env, phi, eff), S.empty_epsilon
      end
    | _ -> raise (TypingBug "pack called with wrong number of arguments")
    (*(input_env, input_phi, input_effect), S.empty_epsilon*)
  end

let handle_alloc (_: exp list)
                 (lvo: lval option)
                 (_: (tau*uniq) list)
                 (input_env: env)
                 (input_phi: phi)
                 (input_effect: effect)
                 (input_lock_effect: lock_effect)
                 : gamma * S.epsilon =
  let alloc_rho = make_rho (LN.Const "alloc") true in
  match lvo with
    None -> raise (TypingBug "calling alloc without assigning the result")
  | Some(lv) ->
      let ((lv_type, _, _), lv_env, lv_phi, lv_effect) =
        type_lval lv input_env input_phi input_effect (*false*) in
      (match lv_type.t with
        ITPtr(var_tref, var_rho) ->
          allocate !var_tref;
          mark_global_tau !var_tref KMalloc_Addr;
          rho_flows alloc_rho var_rho;
          mark_global_rho alloc_rho KMalloc_Addr RhoSet.empty;
      | _ ->
          ignore (warn "alloc result assigned to a non-pointer: %a"
                 d_tau lv_type);
      );
      (lv_env, lv_phi, lv_effect), S.empty_epsilon

let handle_free (_: exp list)
                (_: lval option)
                (args: (tau*uniq) list)
                (input_env: env)
                (input_phi: phi)
                (input_effect: effect)
                (input_lock_effect: lock_effect)
                : gamma * S.epsilon =
  match args with
    ({t=(ITPtr(s,r))},u)::_ ->
      write_rho r input_phi input_effect u;
      visit_concrete_tau (fun r -> write_rho r input_phi input_effect u) !s;
      (input_env, input_phi, input_effect), S.empty_epsilon
  | _ -> raise (TypingBug "calling free with bad arguments")

let get_special (k: handler) : special_function_t =
  match k with
    Alloc -> handle_alloc
  | Free -> handle_free
  | Newlock -> handle_newlock
  | Destroy -> handle_destroylock
  | Acquire -> handle_acquire
  | Trylock -> handle_trylock
  | Release -> handle_release
  | Fork -> handle_fork
  | Exit -> handle_exit
  | Memcpy -> handle_memcpy
  | Strcmp -> handle_strcmp
  | Va_start -> handle_va_start
  | Va_arg -> handle_va_arg
  | Memset -> handle_memset
  | Pack -> handle_pack
  | Start_unpack -> handle_start_unpack
  | End_unpack -> handle_end_unpack
    

(*****************************************************************************)
let rec type_instr input_lock_effect
                   ((input_env, input_phi, input_effect), input_epsilon)
                   instr =
  if !debug then ignore(E.log "typing instruction %a\n" d_instr instr);
  if !do_uniq then
    begin
      if !debug then
        ignore(E.log "(before) uniqueness: %a\n" Uniq.d_state !current_uniqueness);
      fill_thread_local input_env; (* OBSOLETE *)
      (* FIX: this is conservative; we are using the uniqueness from
         AFTER the instruction while typing the instruction itself.
         The fix would be to rename this function type_instr' and then
         create a separate type_instr that first calls this function
         and then updates the uniqueness. *)
      current_uniqueness := Uniq.through_instr !current_uniqueness instr;
      if !debug then
        ignore(E.log "(after) uniqueness: %a\n" Uniq.d_state !current_uniqueness);
    end;
  match instr with
    Set(lv, e, loc) ->
      let e =
        if !do_ignore_casts then e else
        match e with
        | CastE(_,e) -> e
        | _ -> e
      in
      currentLoc := loc;
      let ((s1,_), exp_env, exp_phi, exp_effect) =
        type_exp e input_env input_phi input_effect in
      let ((s2, r, u), lval_env, lval_phi, lval_effect) =
        type_lval lv exp_env exp_phi exp_effect in
      (* ignore(E.log "lval |%a| is %s\n" d_lval lv (uniq2str u)); *)
      sub_tau s1 s2;
      write_rho r lval_phi lval_effect u;
      (lval_env, lval_phi, lval_effect), input_epsilon
  | Call(lvo, e, el, loc) -> begin
      currentLoc := loc;
      let (args, arg_env, arg_phi, arg_effect) =
        type_exp_list el input_env input_phi input_effect in
      try (* if it's in !special_functions, use the corresponding handler *)
        (match e with
          Lval(Var(f),_) ->
            let special = get_special (Strmap.find f.vname !special_functions) in
            special el lvo args arg_env arg_phi arg_effect input_lock_effect
        | _ ->
          raise Not_found
        )
      with Not_found -> begin
      (* not special *)
      let ((e_type,_), e_env, e_phi, e_effect) =
        type_exp e arg_env arg_phi arg_effect
      in
      match e_type.t with
        ITFun fi -> begin
          let callargs = List.map (fun (s,_) -> ("", s)) args in
          let callrett, call_env, call_inphi, call_ineff =
            match lvo with
              None -> fi.fd_output_tau, e_env, e_phi, e_effect
            | Some(lv) ->
                let ((sret, r, u), lv_env, lv_phi, lv_effect) =
                  type_lval lv e_env e_phi e_effect (*false*) in
                write_rho r lv_phi lv_effect u;
                sret, lv_env, lv_phi, lv_effect
          in
          let callargts = List.map (fun (s,_) -> s.ts) args in
          let callts = STFun(callrett.ts,callargts) in
          let ret_phi =
            make_phi "ret" (PhiSplitReturn(fi.fd_lock_effect, call_inphi))
          in
          let call_phi =
            make_phi "call" (PhiSplitCall(fi.fd_lock_effect, ret_phi, !Cil.currentLoc))
          in
          let out_phi = make_phi "out" PhiVar in
          CF.phi_flows call_inphi call_phi;
          CF.phi_flows ret_phi out_phi;
          let callt = make_tau (ITFun {
            fd_arg_tau_list = callargs;
            fd_lock_effect = input_lock_effect;
            fd_input_phi = call_phi;
            fd_input_effect = call_ineff;
            fd_output_tau = callrett;
            fd_output_phi = ret_phi;
            fd_output_effect = fi.fd_output_effect;
            fd_epsilon = S.make_var_epsilon ();
            fd_chi = !current_chi;
          }) callts in
          sub_tau e_type callt;
          (call_env, out_phi, fi.fd_output_effect),
           S.uplus input_epsilon fi.fd_epsilon
        end
      | _ ->
        if !debug then ignore (E.log "call crash\n");
        raise (TypingBug "calling non-function type")
      end
    end
  | Asm(_,_,outlist, inlist,_,loc) ->
      currentLoc := loc;
      let rasm = make_rho (LN.Const "asm") false in
      let (env,phi,eff) = List.fold_left
        (fun (env, phi, eff) (_,e) ->
          let ((s1,_), oenv, ophi, oeff) = type_exp e env phi eff in
          let phi_in = make_phi "conflated_in" PhiVar in
          let phi_out = make_phi "conflated_out" PhiVar in
          conflate_from rasm s1 phi_in phi_out TauSet.empty;
          (oenv, ophi, oeff))
        (input_env, input_phi, input_effect)
        inlist
      in
      let (env, phi, eff) = List.fold_left
        (fun (env, phi, eff) (_,lv) ->
          let ((s1, r,_), oenv, ophi, oeff) = type_lval lv env phi eff (*false*) in
          unify_rho r rasm;
          let phi_in = make_phi "conflated_in" PhiVar in
          let phi_out = make_phi "conflated_out" PhiVar in
          conflate_to s1 rasm phi_in phi_out TauSet.empty;
          (oenv, ophi, oeff))
        (env, phi, eff) outlist
      in
      add_to_read_effect rasm eff;
      add_to_write_effect rasm eff;
      (env, phi, eff), input_epsilon


(*****************************************************************************)

and type_stmt_list g stmts input_lock_effect : gamma * S.epsilon =
  let f (g,e) s =
    let g',e' = type_stmt g input_lock_effect s in
    g', S.uplus e e'
  in
  List.fold_left f (g, S.empty_epsilon) stmts

and type_stmt (env, phi, eff) input_lock_effect stmt : gamma * S.epsilon =
  if !debug then ignore(E.log "typing statement %a\n" d_stmt stmt);
  if !do_uniq then current_uniqueness := Uniq.get_stmt_state stmt;
  (*current_liveness := LV.get_stmt_live_vars stmt;*)
(*
  ignore(E.log "Live vars: ");
  Lockdefs.Strset.iter (fun s -> ignore(E.log "%s, " s)) (!current_liveness);
  ignore(E.log "\n");
  let (_,rs) = find_live_vars env !current_liveness in
  ignore(E.log "Live rhos: %a\n" d_rhoset rs);
*)
  set_goto_target env phi eff stmt;
  let (env,phi,eff) = Hashtbl.find env.goto_tbl stmt in
  match stmt.skind with
    Instr(il) ->
      List.fold_left
        (type_instr input_lock_effect) ((env, phi, eff), S.empty_epsilon) il
  | Return(e, loc) ->
      currentLoc := loc;
      let ((sret,_), env, phi, eff) =
        match e with
          None -> ((void_tau,NotUnq), env, phi, eff)
        | Some(e1) -> type_exp e1 env phi eff
      in
      let (t,r) = env_lookup (!current_function).svar.vname env in begin
        match t.t with
          ITAbs(tref) -> begin
            match !tref.t with
              ITFun fi ->
                CF.phi_flows phi fi.fd_output_phi;
                sub_tau sret fi.fd_output_tau;
                effect_flows fi.fd_output_effect eff;
                (env,
                (* we're making this empty because this will be a
                   dead path, with nothing to connect to it. *)
                CF.empty_phi,
                (* make_phi "p" PhiVar, *)
                (* i think phi, not fresh phi is what should be there,
                   after a return the state of locks doesn't change,
                   there could be a merge with some "live" path later on
                *)
                (* phi, *)
                make_effect "after-return" false),
                 S.empty_epsilon
            | _ -> raise (TypingBug "type of current_function must be ITFun")
          end
        | _ -> raise (TypingBug "type of current_function must be ITFun")
      end
  | Goto(stmt_ref, loc) ->
      currentLoc := loc;
      (*let x = S.make_var_epsilon () in*)
      set_goto_target env phi eff !stmt_ref;
      (env, CF.empty_phi, make_effect "goto-effect" false), S.empty_epsilon
  | If(cond_exp, block1, block2, loc) ->
      currentLoc := loc;
      let ((cond_type,_), env, phi, eff) =
        type_exp cond_exp env phi eff in
      let true_phi,false_phi =
        match cond_type.t with
          ITTrylockInt(l,true) ->
            let acqphi = make_phi "acquire" (PhiAcquire l) in
            CF.phi_flows phi acqphi;
            acqphi, phi
        | ITTrylockInt(l,false) ->
            let acqphi = make_phi "acquire" (PhiAcquire l) in
            CF.phi_flows phi acqphi;
            phi, acqphi
        | _ -> phi, phi
      in
      let (g1,e1) =
        (* invalidates current_liveness *)
        type_stmt_list (env, true_phi, eff) block1.bstmts input_lock_effect
      in
      let (g2,e2) =
        type_stmt_list (env, phi, eff) block2.bstmts input_lock_effect
      in
      let x =  join_gamma g1 g2 in x, S.union e1 e2
  | Loop(b, loc, Some(_), Some(_)) ->
      currentLoc := loc;
      let begin_phi = make_phi "beginloop" PhiVar in
      CF.phi_flows phi begin_phi;
      let (env2,p2,ef2),e =
        (* this typing will side-effect the current_liveness ref.
         * don't use current_liveness from here on. *)
        type_stmt_list (env, begin_phi, eff) b.bstmts input_lock_effect
      in
      CF.phi_flows p2 begin_phi;
      effect_flows eff ef2;
      effect_flows ef2 eff;
      env_flows env2 env;
      (* attn: don't use !current_liveness here because it will refer to the
       * last statement typed. (see above comment)
       *)
      let live_vars = LV.get_stmt_live_vars stmt in
      defer (fun () -> down e env live_vars S.empty_epsilon);
      (*S.epsilon_flow e S.empty_epsilon;*)
      (env2, p2, ef2), S.empty_epsilon
  | Block(b) ->
      type_stmt_list (env, phi, eff) b.bstmts input_lock_effect
  | Loop(_,_,None,_)
  | Loop(_,_,_,None)
  | Break _
  | Continue _
  | Switch _ -> assert false
  | TryExcept _ (* wrong compiler *)
  | TryFinally _ -> assert false

let addvars (varlist: varinfo list) env : (string * tau) list * env =
  let is_glob = function
      Static -> true
    | _ -> false
  in
  let envref = ref env in
  let typelist =
    List.map
      (fun v ->
        Cil.currentLoc := v.vdecl;
        let name = LN.Const v.vname in
        let t = annotate v.vtype [] name in
        let r =
          if v.vaddrof then (
            allocate t;
            make_rho (LN.AddrOf name) true;
          )
          else unknown_rho
        in
        if is_glob v.vstorage then
          begin
            mark_global_tau t KGlobal;
            mark_global_rho r KGlobal RhoSet.empty;
          end;
        envref := env_add_var !envref v.vname (t, r);
        if !debug then ignore(E.log "%s: %a\n" v.vname d_tau t);
        (v.vname, t))
      varlist
  in
    (typelist,!envref)

let addfun (fd: fundec) : unit = begin
  if !debug then ignore (E.log "typing function %s %s\n" fd.svar.vname (Lprof.timestamp ()));
  let location1 = !Cil.currentLoc in
  LV.compute_live_vars fd;
  let location2 = !Cil.currentLoc in
  if !do_uniq then Uniq.compute_uniqueness fd;
  if !debug then ignore (E.log "uniqueness done %s\n" (Lprof.timestamp ()));

  (* if the function doesn't fork, we can reuse one single
   * effect variable for the whole body. *)
  let use_one_effect = !do_one_effect
    && not (Hashtbl.mem functions_that_call_fork fd.svar.vname)
  in
  if use_one_effect then begin
    if !debug_one_effect then ignore(E.log "function %s doesn't call fork\n" fd.svar.vname);
    let e = LF.make_effect (fd.svar.vname^"-singleton") false in
    force_effect e;
  end
  else if !debug_one_effect then ignore(E.log "function %s calls fork\n" fd.svar.vname);

  (* actual work *)
  begin
  match fd.svar.vtype with
    TFun(tret,_,b,_) ->
      current_function := fd;
      Cil.currentLoc := location1;
      let name = LN.Const fd.svar.vname in
      let ret_type = annotate tret [] name in
      let in_phi = make_phi (fd.svar.vname) PhiVar in
      if fd.svar.vname = "main" then
        CF.starting_phis := in_phi::!CF.starting_phis;
      let in_eff = make_effect (fd.svar.vname^"-input") false in
      let start_env = fresh_env () in
      let epsilon = S.make_var_epsilon () in
      let (arg_types, arg_env) = addvars fd.sformals start_env in
      let vlt = make_tau (ITBuiltin_va_list(make_rho (LN.Const "va_list") false)) STBuiltin_va_list in
      let arg_types = 
        if b then (arg_types@ ([("",vlt)]))
             else if (List.length arg_types) = 0
                  then [("",void_tau)]
                  else arg_types
      in
      let const_rho = make_rho (LN.AddrOf name) true in
      let fun_ts = STFun(ret_type.ts, List.map (fun (_,x)-> x.ts) arg_types) in
      let input_lock_effect = make_lock_effect () in
      current_chi := make_chi "X";
      if is_atomic fd.svar then set_atomic_chi !current_chi;
      Cil.currentLoc := location2;
      let out_phi = make_phi (fd.svar.vname^"_out") PhiVar in
      let out_eff = make_effect (fd.svar.vname^"-output") false in
      let fun_type = make_tau (ITFun {
        fd_arg_tau_list = arg_types;
        fd_lock_effect = input_lock_effect;
        fd_input_phi = in_phi;
        fd_input_effect = in_eff;
        fd_output_tau = ret_type;
        fd_output_phi = out_phi;
        fd_output_effect = out_eff;
        fd_epsilon = epsilon;
        fd_chi = !current_chi;
      }) fun_ts in
      let fun_abs_type = make_tau (ITAbs (ref fun_type)) (STAbs fun_ts) in

      (* add polymorphic type to global environment *)
      (try
        let (old_type, old_rho) = env_lookup fd.svar.vname !global_env in
        unify_types old_type fun_abs_type;
        unify_rho old_rho const_rho
      with Not_found -> ());
      global_env := env_add_var !global_env fd.svar.vname
                                (fun_abs_type, const_rho);
      (* add locals to the type-environment of the function *)
      let (_, locals_env) = addvars fd.slocals arg_env in
      (* add universal type of function (for polymorphic recursion) *)
      let final_env =
        env_add_var locals_env fd.svar.vname (fun_abs_type, const_rho)
      in
      (* using this Gamma, type the function body *)
      let (_, phi_stmt, eff_stmt), e =
        type_stmt_list (final_env, in_phi, in_eff) 
                       fd.sbody.bstmts input_lock_effect in
      (* flow the output phi, eff, to the formal output phi, eff *)
      effect_flows out_eff eff_stmt;
      CF.phi_flows phi_stmt out_phi;
      S.epsilon_flow e epsilon;
      current_function := Cil.dummyFunDec;
      if !debug then ignore (E.log "function %s: %a\n" fd.svar.vname d_tau fun_abs_type);
  | _ -> assert false
  end;

  (* if we had forced one effect for the whole function,
   * then restore to using flow-sensitive effects again *)
  if use_one_effect then unforce_effect ();
end

let propagate_vinfo_i (vi1: vinfo)
                      (vi2: vinfo)
                      (i: instantiation)
                      : unit =
  let v1 = U.deref vi1 in
  let v2 = U.deref vi2 in
  if !debug_void then ignore(E.log "propagate_vinfo_i: #%d to #%d through %a\n" v1.vinfo_id v2.vinfo_id d_instantiation i);
  if !do_void_conflate || !do_void_single then begin
    inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) true i;
    inst_rho (getSome v1.vinfo_rho) (getSome v2.vinfo_rho) false i;
    CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) true i;
    CF.inst_phi (getSome v1.vinfo_phi_in) (getSome v2.vinfo_phi_in) false i;
    CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) true i;
    CF.inst_phi (getSome v1.vinfo_phi_out) (getSome v2.vinfo_phi_out) false i;
  end else ();
  if !do_void_single && (v1.vinfo_types = ConflatedRho || v2.vinfo_types = ConflatedRho) then begin
    conflate_vinfo v1;
    conflate_vinfo v2;
  end;
  let name = LN.Const "void" in (* TODO: make this an input *)
  begin
    List.iter
      (fun (ts, t1) ->
        try
          let tl2 = get_vinfo_types v2.vinfo_types in
          let t2 = List.assoc ts tl2 in
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
        with Not_found -> begin
          Cil.currentLoc := v2.vinfo_loc;
          let t2 = reannotate t1 v2.vinfo_known name in
          if !debug_void then ignore (E.log "void: adding %a to v%d\n" d_tau t2 v2.vinfo_id);
          Cil.currentLoc := locUnknown;
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
          add_type_to_vinfo t2 v2;
          Q.add vi2 worklist_vinfo;
        end)
      (get_vinfo_types v1.vinfo_types);
    List.iter
      (fun (ts, t2) ->
        try
          let tl1 = get_vinfo_types v1.vinfo_types in
          (* if it's there, it was instantiated above, so no need to do anything *)
          let _ = List.assoc ts tl1 in ()
        with Not_found -> begin
          Cil.currentLoc := v1.vinfo_loc;
          let t1 = reannotate t2 v1.vinfo_known name in
          Cil.currentLoc := locUnknown;
          instantiate t1 t2 true i;
          instantiate t1 t2 false i;
          add_type_to_vinfo t1 v1;
          Q.add vi1 worklist_vinfo;
        end)
      (get_vinfo_types v2.vinfo_types);
  end

let propagate_cinfo_i (ci1: cinfo) (ci2: cinfo) (i: instantiation): unit =
  let c1 = U.deref ci1 in
  let c2 = U.deref ci2 in
  StrHT.iter
    (fun f (r1,t1) ->
      try
        let (r2,t2) = StrHT.find c2.cinfo_fields f in
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
      with Not_found -> begin
        if !debug_void then ignore(E.log "cinfo: adding %s to %d\n" f c2.cinfo_id);
        Cil.currentLoc := c2.cinfo_loc;
        let t2 = reannotate t1 c2.cinfo_known (LN.Field (c2.cinfo_label_name, f)) in
        let r2 = make_rho (LN.Field (c2.cinfo_label_name, f)) false in
        Cil.currentLoc := locUnknown;
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
    if !debug_void then ignore(E.log "propagate_cinfo_i: instantiate %s on %a:\n   %a\n   %a\n" f d_instantiation i d_tau t1 d_tau t2);
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        StrHT.add c2.cinfo_fields f (r2,t2);
        Q.add ci2 worklist_cinfo;
      end
    ) c1.cinfo_fields;
  StrHT.iter
    (fun f (r2,t2) ->
      try
        (*if it's there, it was instantiated above, so no need to do anything*)
        let _ = StrHT.find c1.cinfo_fields f in ()
      with Not_found -> begin
        if !debug_void then ignore(E.log "adding %s to %d\n" f c1.cinfo_id);
        Cil.currentLoc := c1.cinfo_loc;
        let name = (LN.Field (c1.cinfo_label_name, f)) in
        let t1 = reannotate t2 c1.cinfo_known name in
        let r1 = make_rho name false in
        Cil.currentLoc := locUnknown;
        inst_rho r1 r2 true i;
        inst_rho r1 r2 false i;
        instantiate t1 t2 true i;
        instantiate t1 t2 false i;
        StrHT.add c1.cinfo_fields f (r1,t1);
        Q.add ci1 worklist_cinfo;
      end
    ) c2.cinfo_fields;
  ()
  
let dump_vinfo_stats () : unit =
  ignore(E.log "calculating void* statistics\n");
  let (t,c,e,s) = List.fold_left
    (fun (t,c,e,s) v ->
      let xd = U.deref v in
      match List.length (get_vinfo_types xd.vinfo_types) with
        0 -> (t+1,c,e+1,s)
      | 1 -> (t+1,c,e,s+1)
      | _ -> (t+1,c+1,e,s))
    (0,0,0,0)
    !all_vinfo
  in
  ignore(E.log "Void-type-analysis statistics\n");
  ignore(E.log "total     : %d\n" t);
  ignore(E.log "empty     : %d\n" e);
  ignore(E.log "singleton : %d\n" s);
  ignore(E.log ">=2       : %d\n" c);
  ignore(E.log "\n")

let dump_cinfo_stats () : unit =
  ignore(E.log "calculating struct statistics\n");
  let (t,tf,uf) = List.fold_left
    (fun (t,tf,uf) c ->
      let cd = U.deref c in
      (t+1,
      tf + (List.length cd.compinfo.cfields),
      uf+(StrHT.length cd.cinfo_fields)))
    (0,0,0)
    !all_cinfo
  in
  ignore(E.log "Struct-field optimization statistics\n");
  ignore(E.log "total struct types: %d\n" t);
  ignore(E.log "total fields      : %d\n" tf);
  ignore(E.log "used fields       : %d\n" uf);
  ignore(E.log "\n")

let solve_vinfo_cinfo_constraints () : unit =
  if !debug_void then ignore(E.log "solve vinfo/cinfo\n");
  (* Step 1:  Solve all vinfo and cinfo constraints simultaneously.
     propagate_X_i will add new edges to the worklist, as will any calls
     to make_X done during resolution *)
  while (not (Q.is_empty worklist_vinfo) || not (Q.is_empty worklist_cinfo)) do
    if !debug then ignore(E.log "worklist size: %d vinfo, %d cinfo\n"
                            (Q.length worklist_vinfo) (Q.length worklist_cinfo));
    if (not (Q.is_empty worklist_vinfo)) then
      let vi1 = Q.take worklist_vinfo in
      begin
        if !debug_void then ignore(E.log "worklist vinfo: %d\n" (U.deref vi1).vinfo_id);
        InstHT.iter
          (fun i vi2 -> propagate_vinfo_i vi1 vi2 i)
          (U.deref vi1).vinfo_inst_out_edges;
        InstHT.iter
          (fun i vi0 -> propagate_vinfo_i vi0 vi1 i)
          (U.deref vi1).vinfo_inst_in_edges;
      end
    else if (not (Q.is_empty worklist_cinfo)) then
      let ci1 = Q.take worklist_cinfo in
      begin
        if !debug_void then ignore(E.log "worklist cinfo: %d\n" (U.deref ci1).cinfo_id);
        InstHT.iter
          (fun i ci2 -> propagate_cinfo_i ci1 ci2 i)
          (U.deref ci1).cinfo_inst_out_edges;
        InstHT.iter
          (fun i ci0 -> propagate_cinfo_i ci0 ci1 i)
          (U.deref ci1).cinfo_inst_in_edges;
      end
  done;
  (* Step 2: ``Allocate'' all the vinfos *)
  List.iter
    (fun x ->
      let xd = U.deref x in
      List.iter
        (fun loc ->
          Cil.currentLoc := loc;
          List.iter
            (fun (_, t) -> allocate t)
            (get_vinfo_types xd.vinfo_types);
          Cil.currentLoc := locUnknown;
        )
        xd.vinfo_alloc
    )
    !all_vinfo;
  (* Step 3:  ``Allocate'' all the cinfos *)
  let scanned = ref [] in
  List.iter
    (fun x ->
      let xd = U.deref x in
      if not (List.memq xd !scanned) then begin
        scanned := xd::!scanned;
        List.iter 
          (fun loc ->
            StrHT.iter
              (fun f (r,t) ->
                let name = (LN.Field (LN.Const "alloc", f)) in
                Cil.currentLoc := loc;
                let rc = make_rho name true in
                unify_rho rc r;
                allocate t;
                Cil.currentLoc := locUnknown;
              )
              xd.cinfo_fields;
          )
          xd.cinfo_alloc;
      end
    )
    !all_cinfo

let filter_quantified (l: attrparam list) : unit =
  let error = (TypingBug "use #pragma(structname.fieldname,<index list>)")
  in
  match l with
    ACons(structname,[])::tl ->
      if !debug then ignore(E.log "existential %s:\n" structname);
      let func = emptyFunction "dummy_func" in
      let t = (Hashtbl.find typenames ("struct "^structname)) in
      let v = makeLocalVar func structname t in
      begin
        match t with
          TComp(c,_) -> ()
        | _ -> assert false
      end;
      List.iter
        (function
          AStr(e) ->
            let exp = Formatcil.cExp e [structname, Fv v] in
            if !debug then ignore(E.log "quantified: %a\n" d_exp exp);
            Hashtbl.add quantified_map structname exp
        | _ -> raise error)
        tl
  | _ -> raise error

class constraintVisitor = object
  inherit nopCilVisitor
  method vglob (g: global) : global list visitAction =
  begin
    match g with
    | GType(_,_) -> DoChildren
    | GCompTag(_, _) -> DoChildren
    | GCompTagDecl(_, _) -> DoChildren
    | GEnumTag(_, _) -> DoChildren
    | GEnumTagDecl(_, _) -> DoChildren
    | GVarDecl(vi, loc) ->
        currentLoc := loc;
        let (var_type, var_rho) =
          try env_lookup vi.vname !global_env
          with Not_found ->
            if Strmap.mem vi.vname !special_functions then begin
              let f = Strmap.find vi.vname !special_functions in
              if f = Alloc then
                match vi.vtype with
                | TFun (t, args, b, al) ->
                    let t' = typeAddAttributes [Attr ("unique",[])] t in
                    vi.vtype <- TFun (t', args, b, al)
                | _ -> assert false
            end;
            let name = (LN.Const vi.vname) in
            let t = annotate vi.vtype [] name in
            let r = (make_rho (LN.AddrOf name) true) in
            match t.t with
              ITFun _ ->
                if (not (List.mem vi.vname !def_functions)) &&
                   (not (Strmap.mem vi.vname !special_functions)) &&
                   (not (List.mem vi.vname !undef_functions))
                then undef_functions := vi.vname::!undef_functions;
                make_tau (ITAbs (ref t)) (STAbs t.ts), r
            | _ -> (t, r)
        in
        mark_global_tau var_type KGlobal;
        mark_global_rho var_rho KGlobal RhoSet.empty;
        global_env := env_add_var !global_env vi.vname (var_type, var_rho);
        if !debug then ignore(E.log "%s : %a\n" vi.vname d_tau var_type);
        DoChildren
    | GVar(vi, ii, loc) -> begin
        currentLoc := loc;
        let name = (LN.Const vi.vname) in
        let var_type =
          let t = annotate vi.vtype [] name in
          match t.t with
            ITFun _ -> make_tau (ITAbs (ref t)) (STAbs t.ts)
          | _ -> allocate t; t
        in
        let var_rho =
          try
            let (old_type, old_rho) = env_lookup vi.vname !global_env in
            unify_types old_type var_type;
            old_rho
          with Not_found -> make_rho (LN.AddrOf name) true
        in
        global_env := env_add_var !global_env vi.vname (var_type, var_rho);
        mark_global_tau var_type KGlobal;
        mark_global_rho var_rho KGlobal RhoSet.empty;
        if !debug then ignore(E.log "%s : %a\n" vi.vname d_tau var_type);
        match ii.init with
        | Some(i) -> begin
            let (init_type,env) = type_init i !global_env name in
            sub_tau init_type var_type;
            if !global_env != env then assert false;
            global_env := env
          end
        | _ -> ()
      end;
        DoChildren
    | GFun(fd, loc) ->
        begin
          currentLoc := loc;
          if (List.mem fd.svar.vname !undef_functions) then
            undef_functions :=
              List.filter (fun x -> x <> fd.svar.vname) !undef_functions;
          if (List.mem fd.svar.vname !def_functions) then
            ignore(E.error "function %s defined twice!\n" fd.svar.vname);
          assert (not (List.mem fd.svar.vname !def_functions));
          def_functions := fd.svar.vname::!def_functions;
          addfun fd;
          DoChildren
        end
    | GAsm(_, _) -> DoChildren
    | GPragma(a, _) -> begin
        match a with
          Attr(at,l) when at = pragmaKeyword ->
            if !do_existentials then filter_quantified l;
        | _ -> ()
      end;
      DoChildren
    | GText(_) -> DoChildren
  end
end

let handle_undef_function (name: string) : unit =
  assert (Strmap.mem name (!global_env).var_map);
  let (t,_) = env_lookup name !global_env in
  match t.t with
    ITAbs(tref) -> begin
      match !tref.t with
        ITFun fi ->
          (* make "id", don't touch arguments *)
          CF.phi_flows fi.fd_input_phi fi.fd_output_phi;
          (*CF.phi_flows fi.fd_output_phi fi.fd_input_phi;*)
          effect_flows fi.fd_output_effect fi.fd_input_effect;
          (*effect_flows fi.fd_input_effect fi.fd_output_effect;*)
          (*fd_arg_tau_list: (string * tau) list;
          mutable fd_output_tau: tau;*)
      | _ -> assert false
    end
  | _ -> assert false

let init f : unit = begin
  if !debug then ignore(E.log "unroll types\n");
  let tv = new unrollTypeVisitor in
  visitCilFileSameGlobals tv f;
  List.iter
    (fun s ->
      try
        let locktype = (Hashtbl.find typenames s) in
        let locktypesig = Cil.typeSig locktype in
        locktypes := locktype::!locktypes;
        locktypesigs := locktypesig::!locktypesigs;
      with Not_found -> ()
    )
    !lock_type_names;

  global_env := {
    goto_tbl = Hashtbl.create 1;
    var_map =
      Hashtbl.fold 
        (fun name (resTyp, argTypes, isva) (h: (tau*rho) Strmap.t) ->
          let t = (TFun(resTyp,
                 Some (List.map (fun at -> ("", at, [])) argTypes),
                 isva, [])) in
          let ts = typ_tau_sig t in
          let tau = make_tau (ITAbs (ref(annotate t [] (LN.Const name)))) ts in
          if not (Strmap.mem name !special_functions)
          then undef_functions := name::!undef_functions;
          Strmap.add name (tau,unknown_rho) h)
        Cil.gccBuiltins Strmap.empty;
    unpacked_map = Strmap.empty; 
  };

  (* find all functions that call fork *)
  if !debug then ignore(E.log "find what calls fork\n");
  let rec dfs (node: CG.callnode) : unit =
    if not (Hashtbl.mem functions_that_call_fork node.CG.cnInfo.vname) then begin
      Hashtbl.add functions_that_call_fork node.CG.cnInfo.vname ();
      Hashtbl.iter (fun _ n -> dfs n) node.CG.cnCallers;
    end
  in
  let graph: CG.callgraph = (CG.computeGraph f) in
  (*Hashtbl.clear functions_that_call_fork;*)
  Hashtbl.iter
    (fun s n ->
      try
        if (Strmap.find s !special_functions) = Fork
        then dfs n
        else ()
      with Not_found -> ())
    graph;
  (*if !debug_one_effect then 
    Hashtbl.iter (fun s _ -> ignore(E.log "function %s calls fork\n" s)) functions_that_call_fork;*)
  if !debug then ignore(E.log "found what calls fork\n");
end

let generate_constraints (f: file) : unit = begin
  init f;

  if !debug then ignore(E.log "typing-parsing\n");
  let cv = new constraintVisitor in visitCilFileSameGlobals cv f;
  Lprof.endtime "typing-parsing";
  if !debug then ignore(E.log "typing-parsing done\n");
  Cil.currentLoc := locUnknown;

  if !debug then ignore(E.log "solve void* and lazy-struct-field constraints\n");
  solve_vinfo_cinfo_constraints ();
  Lprof.endtime "typing-void*";
  if !debug then ignore(E.log "solve void* and lazy-struct-field constraints done\n");

  if !debug then ignore(E.log "set globals\n");
  set_globals ();
  Lprof.endtime "typing-globals";
  if !debug then ignore(E.log "set globals done\n");

  if !debug then ignore(E.log "applying down\n");
  done_typing();
  (*apply_down ();*)
  Lprof.endtime "typing-down";
  if !debug then ignore(E.log "applying down done\n");

  if !debug then ignore(E.log "constraint generation done\n");

  if (List.length !undef_functions) > 0 then
    ignore(E.log "functions declared but not defined:\n");
  undef_functions := List.sort Pervasives.compare !undef_functions;
  List.iter
    (fun s -> ignore(E.log "  %s\n" s); handle_undef_function s)
    !undef_functions;
  if !debug then begin
    ignore(E.log "solved void variables:\n");
    dump_vinfo_stats ();
    dump_cinfo_stats ();
  end;
  clear_globals();
end
