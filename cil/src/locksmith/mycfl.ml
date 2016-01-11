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
open Printf

module H = Hashtbl
module Q = Queue

type location = Cil.location

type instantiation = int
let inst_compare = (-)

type 'a node_t = {
  node_id: int;
  node_data: 'a;
}

let hash x = x.node_id

type edge_kind = 
  | EdgeS
  | EdgeP
  | EdgeN
  | EdgeM
  | Edgeg
  | EdgeG
  (*| Edgep
  | Edgen*)
  | Edgee
  | EdgeK of instantiation
  | Edgeopeni of instantiation
  | Edgeclosei of instantiation
  | Edged

let knd2str knd =
  match knd with
  | EdgeS -> "S"
  | EdgeP -> "P"
  | EdgeN -> "N"
  | EdgeM -> "M"
  | Edgeg -> "g"
  | EdgeG -> "G"
  (*| Edgep -> "p"
  | Edgen -> "n"*)
  | Edgee -> "e"
  | EdgeK(i) -> "K" ^ (string_of_int i)
  | Edgeopeni(i) -> "(" ^ (string_of_int i)
  | Edgeclosei(i) -> ")" ^ (string_of_int i)
  | Edged -> "d"

module Node =
  struct
    type 'a t = 'a node_t
    let compare x y = x.node_id - y.node_id
  end
module NodeSet = Setp.Make(Node)

type 'a edge_t = {
  edge_kind           : edge_kind;
  edge_source         : 'a node_t;
  edge_target         : 'a node_t;
}

module Edge =
  struct
    type 'a t = 'a edge_t
    let compare x y =
      if (Node.compare x.edge_source y.edge_source) = 0
      then
        if (Node.compare x.edge_target y.edge_target) = 0
        then Pervasives.compare x.edge_kind y.edge_kind
        else Node.compare x.edge_target y.edge_target
      else Node.compare x.edge_source y.edge_source
  end
module EdgeSet = Setp.Make(Edge)

type node_data = {
  mutable node_name       : string;
  node_location           : location;
  mutable node_global     : bool;
  node_concrete           : bool;
  mutable node_succ_s     : node_data NodeSet.t;
  mutable node_pred_s     : node_data NodeSet.t;
  mutable node_succ_p_m_p : node_data NodeSet.t;
  mutable node_pred_p_m_p : node_data NodeSet.t;
  mutable node_succ_p_p_p : node_data NodeSet.t;
  mutable node_pred_p_p_p : node_data NodeSet.t;
  mutable node_succ_n_m_n : node_data NodeSet.t;
  mutable node_pred_n_m_n : node_data NodeSet.t;
  mutable node_succ_n_n_n : node_data NodeSet.t;
  mutable node_pred_n_n_n : node_data NodeSet.t;
  mutable node_succ_k_m_i : (instantiation, node_data NodeSet.t) H.t;
  mutable node_pred_k_m_i : (instantiation, node_data NodeSet.t) H.t;
  mutable node_succ_m_i_k : (instantiation, node_data NodeSet.t) H.t;
  mutable node_pred_m_i_k : (instantiation, node_data NodeSet.t) H.t;
  mutable node_succ_m_m_m : node_data NodeSet.t;
  mutable node_pred_m_m_m : node_data NodeSet.t;
  mutable node_succ_g_i   : node_data NodeSet.t;
  mutable node_pred_g_i   : node_data NodeSet.t;
  mutable node_succ_i_g   : node_data NodeSet.t;
  mutable node_pred_i_g   : node_data NodeSet.t;
}
type nodeset = node_data NodeSet.t
type edgeset = node_data EdgeSet.t

module HashedType =
  struct
    type t = node_data node_t
    let equal n1 n2 =
      let h1 = hash n1 in  (* assumes unique hashing *)
      let h2 = hash n2 in
      h1 == h2
    let hash = hash
  end

type node = node_data node_t
type edge = node_data edge_t

let is_terminal (e: edge) : bool =
  match e.edge_kind with
  | EdgeS
  | EdgeP
  | EdgeN
  (*| Edgep
  | Edgen*)
  | EdgeM
  | EdgeK(_)
  | EdgeG -> false
  | Edgeg
  | Edgee
  | Edgeopeni(_)
  | Edgeclosei(_)
  | Edged -> true

let is_concrete (n: node) : bool = n.node_data.node_concrete
let is_global (n: node) : bool = n.node_data.node_global

let inst_index : int ref = ref 0
let fresh_inst () : int =
  incr inst_index;
  !inst_index

let node_index = ref 0

let total_nodes () = !node_index

let insts = ref []
let nodes = ref NodeSet.empty
let edges = ref EdgeSet.empty
let globals = ref NodeSet.empty

let make_instantiation () : instantiation =
  let id = fresh_inst() in
  insts := id::!insts;
  id

let make_node (name: string) (concrete: bool) (loc: location): node = begin
  let n = {
    node_name       = name;
    node_concrete   = concrete;
    node_global     = false; (* set this to true using set_global *)
    node_location   = loc;
    node_succ_s     = NodeSet.empty;
    node_pred_s     = NodeSet.empty;
    node_succ_p_m_p = NodeSet.empty;
    node_pred_p_m_p = NodeSet.empty;
    node_succ_p_p_p = NodeSet.empty;
    node_pred_p_p_p = NodeSet.empty;
    node_succ_n_m_n = NodeSet.empty;
    node_pred_n_m_n = NodeSet.empty;
    node_succ_n_n_n = NodeSet.empty;
    node_pred_n_n_n = NodeSet.empty;
    node_succ_k_m_i = H.create 10;
    node_pred_k_m_i = H.create 10;
    node_succ_m_i_k = H.create 10;
    node_pred_m_i_k = H.create 10;
    node_succ_m_m_m = NodeSet.empty;
    node_pred_m_m_m = NodeSet.empty;
    node_succ_g_i   = NodeSet.empty;
    node_pred_g_i   = NodeSet.empty;
    node_succ_i_g   = NodeSet.empty;
    node_pred_i_g   = NodeSet.empty;
  } in
  incr node_index;
  let i = { node_id = !node_index; node_data = n } in
  nodes := NodeSet.add i !nodes;
  i
end

let string_of_loc l =
  Pretty.sprint 80 (Cil.d_loc () l)

let string_of_inst = string_of_int

let string_of_node n = n.node_data.node_name ^ "@"
                     ^ (string_of_loc n.node_data.node_location)
let dotstring_of_node n = n.node_data.node_name
                     ^ (string_of_int n.node_id) ^ "\\n"
                     ^ (string_of_loc n.node_data.node_location)
let string_of_edge e = 
  knd2str e.edge_kind
  ^ " from " ^ (string_of_loc e.edge_source.node_data.node_location)
  ^ " to " ^ (string_of_loc e.edge_target.node_data.node_location)

let print_edge e = begin
  print_string (knd2str e.edge_kind);
  print_string (" from " ^ (string_of_loc e.edge_source.node_data.node_location));
  print_string (" to " ^ (string_of_loc e.edge_target.node_data.node_location));
  (*print_newline();*)
end

let make_edge (knd: edge_kind) (src: node) (tgt: node) (*glob: bool*) = begin
  let e = {
    edge_kind = knd;
    edge_source = src;
    edge_target = tgt;
  } in
  if not (EdgeSet.mem e !edges) then begin
    edges := EdgeSet.add e !edges;
  end;
  e
end

let make_sub_edge (src: node) (tgt: node) : unit =
  ignore(make_edge Edged src tgt)
let make_open_edge (src: node) (tgt: node) (i: instantiation) : unit =
  ignore(make_edge (Edgeopeni i) src tgt)
  (*ignore(make_edge Edgen src tgt)*)
let make_close_edge (src: node) (tgt: node) (i: instantiation) : unit =
  ignore(make_edge (Edgeclosei i) src tgt)
  (*ignore(make_edge Edgep src tgt)*)

let make_inst_edge n1 n2 p i =
  if p then make_close_edge n1 n2 i
  else make_open_edge n2 n1 i

let set_global (n: node) : unit = begin
  let data = n.node_data in
  if data.node_global
  then ()
  else begin
    data.node_global <- true;
    globals := NodeSet.add n !globals;
    ignore(make_edge Edgeg n n);
  end
end

let get_nodeset (i: instantiation) (h: (instantiation, nodeset) H.t) =
  try
    H.find h i 
  with
    | Not_found -> NodeSet.empty


let reaches x y =
  let e = {
    edge_kind = EdgeS;
    edge_source = x;
    edge_target = y;
  } in
  EdgeSet.mem e !edges

let reachesm x y =
  let e = {
    edge_kind = EdgeM;
    edge_source = x;
    edge_target = y;
  } in
  EdgeSet.mem e !edges

let g: edgeset ref = ref EdgeSet.empty
let doFullCFL () = begin
  NodeSet.iter (fun x -> begin
      ignore(make_edge EdgeP x x);
      ignore(make_edge EdgeN x x);
      ignore(make_edge EdgeM x x);
    end) !nodes;
  let q: edge Q.t = Q.create() in
  EdgeSet.iter (fun x -> Q.add x q) !edges;
  (*fprintf stdout "foobari %d\n" !node_index ;
  fprintf stdout "foobar %d\n" (Q.length q) ;*)
  let rec loopCFL () = begin
      if Q.is_empty q then ()
      else let e = Q.take q in
      (*print_string "processing ";
      print_edge e;
      print_newline();*)
      if not (EdgeSet.mem e !g) then
      begin
        g := EdgeSet.add e !g;
        (*fprintf stdout "e = %s\n" (string_of_edge e) ;*)
        match e.edge_kind with
          | EdgeS -> ()
          | EdgeP ->
              begin
                (*S <- P N*)
                e.edge_target.node_data.node_pred_s <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_s;
                let f: node -> unit = (fun n -> Q.add (make_edge EdgeS e.edge_source n) q) in
                NodeSet.iter f e.edge_target.node_data.node_succ_s;
                (*P <- M P*)
                e.edge_source.node_data.node_succ_p_m_p <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_p_m_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP n e.edge_target) q) e.edge_source.node_data.node_pred_p_m_p;
                (*P <- p P*)
                e.edge_source.node_data.node_succ_p_p_p <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_p_p_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP n e.edge_target) q) e.edge_source.node_data.node_pred_p_p_p;
              end
          | EdgeN -> 
              begin
                (*S <- P N*)
                e.edge_source.node_data.node_succ_s <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_s;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeS n e.edge_target) q) e.edge_source.node_data.node_pred_s;
                (*N <- M N*)
                e.edge_source.node_data.node_succ_n_m_n <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_n_m_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN n e.edge_target) q) e.edge_source.node_data.node_pred_n_m_n;
                (*N <- n N*)
                e.edge_source.node_data.node_succ_n_n_n <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_n_n_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN n e.edge_target) q) e.edge_source.node_data.node_pred_n_n_n;
              end
          | EdgeM ->
              begin
                (* P <- M P *)
                e.edge_target.node_data.node_pred_p_m_p <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_p_m_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP e.edge_source n) q) e.edge_target.node_data.node_succ_p_m_p;
                (* N <- M N *)
                e.edge_target.node_data.node_pred_n_m_n <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_n_m_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN e.edge_source n) q) e.edge_target.node_data.node_succ_n_m_n;
                (* Ki<- M )i*)
                let f i = begin
                  let nspred = get_nodeset i e.edge_target.node_data.node_pred_k_m_i in
                  let nssucc = get_nodeset i e.edge_target.node_data.node_succ_k_m_i in
                  H.replace e.edge_target.node_data.node_pred_k_m_i i (NodeSet.add e.edge_source nspred);
                  NodeSet.iter (fun n -> Q.add (make_edge (EdgeK i) e.edge_source n) q) nssucc;
                end
                in List.iter f !insts;
                (* M <- M M *)
                e.edge_target.node_data.node_pred_m_m_m <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_m_m_m;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) e.edge_target.node_data.node_succ_m_m_m;
                e.edge_source.node_data.node_succ_m_m_m <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_m_m_m;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) e.edge_source.node_data.node_pred_m_m_m;
              end
          | EdgeG ->
              begin
                (* g <- (i g *)
                e.edge_source.node_data.node_succ_i_g <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_i_g;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeG n e.edge_target) q) e.edge_source.node_data.node_pred_i_g;
                (* g <- g )i *)
                e.edge_target.node_data.node_pred_g_i <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_g_i;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeG e.edge_source n) q) e.edge_target.node_data.node_succ_g_i;
                (* M <- g *)
                Q.add (make_edge EdgeM e.edge_source e.edge_target) q;
              end
          | Edgeg ->
                (* G <- g *)
                Q.add (make_edge EdgeG e.edge_source e.edge_target) q;
          | Edgee -> ()
          | EdgeK(i) ->
              begin
                (* M <- (i Ki *)
                let nssucc = get_nodeset i e.edge_source.node_data.node_succ_m_i_k in
                let nspred = get_nodeset i e.edge_source.node_data.node_pred_m_i_k in
                H.replace e.edge_source.node_data.node_succ_m_i_k i (NodeSet.add e.edge_target nssucc);
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) nspred;
              end
          | Edgeopeni(i) ->
              begin
                (* M <- (i Ki *)
                let nssucc = get_nodeset i e.edge_target.node_data.node_succ_m_i_k in
                let nspred = get_nodeset i e.edge_target.node_data.node_pred_m_i_k in
                H.replace e.edge_target.node_data.node_pred_m_i_k i (NodeSet.add e.edge_source nspred);
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) nssucc;
                (* g <- (i g *)
                e.edge_target.node_data.node_pred_i_g <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_i_g;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeG e.edge_source n) q) e.edge_target.node_data.node_succ_i_g;
                (* N <- (i N *)
                e.edge_target.node_data.node_pred_n_n_n <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_n_n_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN e.edge_source n) q) e.edge_target.node_data.node_succ_n_n_n;
              end
          | Edgeclosei(i) ->
              begin
                (* Ki <- M )i *)
                let nssucc = get_nodeset i e.edge_source.node_data.node_succ_k_m_i in
                let nspred = get_nodeset i e.edge_source.node_data.node_pred_k_m_i in
                H.replace e.edge_source.node_data.node_succ_k_m_i i (NodeSet.add e.edge_target nssucc);
                NodeSet.iter (fun n -> Q.add (make_edge (EdgeK i) n e.edge_target) q) nspred;
                (* g <- g )i *)
                e.edge_source.node_data.node_succ_g_i <- NodeSet.add e.edge_target e.edge_source.node_data.node_succ_g_i;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeG n e.edge_target) q) e.edge_source.node_data.node_pred_g_i;
                (* P <- )i P *)
                e.edge_target.node_data.node_pred_p_p_p <- NodeSet.add e.edge_source e.edge_target.node_data.node_pred_p_p_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP e.edge_source n) q) e.edge_target.node_data.node_succ_p_p_p;
              end
          | Edged ->
              begin
                Q.add (make_edge EdgeM e.edge_source e.edge_target) q;
              end
      end;
      loopCFL ()
  end
  in
  loopCFL()
end

(* Given a node n and a find function mapping nodes to targets,
   find all elements that reach n, use find to covert them
   to targets, and then use add_node to union them in to the
   original set, returning the resulting set. *)
let get_all_that_reach_m (n: node)
                         (find: node -> 'a)
                         (add_node: 'a -> 'b -> 'b)
                         (set : 'b)
                         : 'b =
  NodeSet.fold
    (fun (x: node) (s: 'b) -> add_node (find x) s)
    n.node_data.node_pred_m_m_m
    set
let get_all_that_reach_pn (n: node)
                         (find: node -> 'a)
                         (add_node: 'a -> 'b -> 'b)
                         (set : 'b)
                         : 'b =
  NodeSet.fold
    (fun (x: node) (s: 'b) -> add_node (find x) s)
    n.node_data.node_pred_s
    set

(* assume this is n1 reaches n2 query *)
let reaches_m (n1: node) (n2: node): bool =
  NodeSet.mem n2 n1.node_data.node_pred_m_m_m

let print_s_edges outf = begin
  (*fprintf outf "digraph G {\n";*)
(*  fprintf outf "size=\"12,12\";\n";*)
  let f e =
    match e.edge_kind with
    | EdgeS -> fprintf outf "%s" ("\"" ^ (dotstring_of_node e.edge_source)
          ^ "\" -> \"" ^ (dotstring_of_node e.edge_target)
          ^ "\" [label=\"" ^ (knd2str e.edge_kind) ^ "\"];\n")
    | _ -> ()
  in
  EdgeSet.iter f !edges;
  (*fprintf outf "}\n";
  close_out outf*)
end

let print_edges outf = begin
  (*let outf = open_out filename in
  fprintf outf "digraph G {\n";*)
  (*fprintf outf "size=\"12,12\";\n";*)
  (*fprintf outf "subgraph \"concretes\" { rank=\"source\";\n";*)
  let nf n = if n.node_data.node_concrete then fprintf outf "\"%s\" [shape=\"box\"]\n" (dotstring_of_node n)
  in
  NodeSet.iter nf !nodes;
  (*fprintf outf "}\n";*)
  let f e = if is_terminal e then fprintf outf "%s" ("\"" ^ (dotstring_of_node e.edge_source)
          ^ "\" -> \"" ^ (dotstring_of_node e.edge_target)
          ^ "\" [label=\"" ^ (knd2str e.edge_kind) ^ "\"];\n")
  in
  EdgeSet.iter f !edges;
  (*fprintf outf "}\n";
  close_out outf*)
end

let print_graph = print_edges

let test () = 
  let d0 = make_node "d0" false Cil.locUnknown in
  let d1 = make_node "d1" false Cil.locUnknown in
  let d2 = make_node "d2" false Cil.locUnknown in
  let d3 = make_node "d3" false Cil.locUnknown in
  let d4 = make_node "d4" false Cil.locUnknown in
  let d5 = make_node "d5" false Cil.locUnknown in
  let d6 = make_node "d6" false Cil.locUnknown in
  let d7 = make_node "d7" false Cil.locUnknown in
  let d8 = make_node "d8" false Cil.locUnknown in
  let d9 = make_node "d9" false Cil.locUnknown in
  let i1 = make_instantiation() in
  let i2 = make_instantiation() in
  let i3 = make_instantiation() in
  begin
  ignore(make_sub_edge d1 d2);
  ignore(make_open_edge d0 d1 i1);
  ignore(make_close_edge d2 d3 i1);
  ignore(make_close_edge d2 d4 i2);
  ignore(make_open_edge d5 d1 i2);
  ignore(make_close_edge d7 d2 i3);
  ignore(make_sub_edge d1 d8);
  ignore(make_close_edge d8 d9 i3);
  set_global d8;

  print_string "solving...";
  print_newline();

  doFullCFL();

  if reaches d1 d1 then print_string "reflexivity ok" else print_string "oops"; print_newline();
  if reaches d1 d2 then print_string "subtype edges ok" else print_string "oops"; print_newline();
  if reaches d0 d3 then print_string "matched paths ok" else print_string "oops"; print_newline();
  if reaches d5 d4 then print_string "matched paths 2 ok" else print_string "oops"; print_newline();
  if not (reaches d0 d4) then print_string "unmatched paths ok" else print_string "!!! unmatched oops"; print_newline();
  if not (reaches d5 d3) then print_string "unmatched paths 2 ok" else print_string "!!! unmatched oops"; print_newline();
  if not (reaches d5 d0) then print_string "non-paths ok" else print_string "oops"; print_newline();
  if (reaches d0 d2) then print_string "pn works" else print_string "pn oops"; print_newline();
  if reaches d5 d2 then print_string "n path ok" else print_string "n oops"; print_newline();
  if reaches d0 d2 then print_string "n path ok" else print_string "n oops"; print_newline();
  if reaches d1 d3 then print_string "p path ok" else print_string "p oops"; print_newline();
  if reaches d2 d4 then print_string "p path ok" else print_string "p oops"; print_newline();
  if reaches d2 d3 then print_string "p path ok" else print_string "p oops"; print_newline();
  if reaches d7 d3 then print_string "p path ok" else print_string "p oops"; print_newline();
  if reaches d7 d2 then print_string "p path ok" else print_string "p oops"; print_newline();
  if reaches d0 d8 then print_string "global ok" else print_string "p oops"; print_newline();
  if reaches d5 d8 then print_string "global ok" else print_string "p oops"; print_newline();
  if reaches d0 d9 then print_string "global ok" else print_string "p oops"; print_newline();
  if reaches d5 d9 then print_string "global ok" else print_string "p oops"; print_newline();
  if not (reaches d7 d9) then print_string "global ok" else print_string "p oops"; print_newline();

end

let done_adding () : unit = ()

(*let _ = test()*)
