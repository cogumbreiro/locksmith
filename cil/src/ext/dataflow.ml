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

module IH = Inthash
module E = Errormsg

open Cil
open Pretty

(** A framework for data flow analysis for CIL code *)

type 't action = 
    Default (* The default action *)
  | Done of 't (* Do not do the default action. Use this result *)
  | Post of ('t -> 't) (* The default action, followed by the given 
                        * transformer *)


(******************************************************************
 **********
 **********         FORWARDS 
 **********
 ********************************************************************)

module type ForwardsTransfer = sig
  val name: string (* For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. May be 
           * imperative.  *)

  val copy: t -> t
  (** Make a deep copy of the data *)


  val stmtStartData: t Inthash.t
  (** For each statement id, the data at the start. Not found in the hash 
   * table means nothing is known about the state at this point. At the end 
   * of the analysis this means that the block is not reachable. *)

  val pretty: unit -> t -> Pretty.doc 
  (** Pretty-print the state *)

  val computeFirstPredecessor: Cil.stmt -> t -> t
  (** Give the first value for a predecessors, compute the value to be set 
   * for the block *)

  val combinePredecessors: Cil.stmt -> old:t -> t -> t option
  (** Take some old data for the start of a statement, and some new data for 
   * the same point. Return None if the combination is identical to the old 
   * data. Otherwise, compute the combination, and return it. *)

  val doInstr: Cil.instr -> t -> t action
  (** The (forwards) transfer function for an instruction. The 
   * {!Cil.currentLoc} is set before calling this. The default action is to 
   * continue with the state unchanged. *)


  val doStmt: Cil.stmt -> t -> unit action
  (** The (forwards) transfer function for a statement. The {!Cil.currentLoc} 
   * is set before calling this. The default action is to continue with the 
   * successors of this block, but only for the ... statements. For other 
   * kinds of branches you must handle it, and return {!Jvmflow.Done}. *)

  val filterStmt: Cil.stmt -> bool
  (** Whether to put this statement in the worklist. This is called when a 
   * block would normally be put in the worklist. *)
  
end


module ForwardsDataFlow = 
  functor (T : ForwardsTransfer) ->
  struct

    (** Keep a worklist of statements to process. It is best to keep a queue, 
     * because this way it is more likely that we are going to process all 
     * predecessors of a statement before the statement itself. *)
    let worklist: Cil.stmt Queue.t = Queue.create ()

    (** We call this function when we have encountered a statement, with some 
     * state. *)
    let reachedStatement (s: stmt) (d: T.t) : unit = 
      (** see if we know about it already *)
      E.pushContext (fun _ -> dprintf "Reached statement %d with %a" 
          s.sid T.pretty d);
      let newdata: T.t option = 
        try
          let old = IH.find T.stmtStartData s.sid in 
          match T.combinePredecessors s ~old:old d with 
            None -> (* We are done here *)
              if !T.debug then begin
                ignore (E.log
                "FF(%s): reached stmt %d with %a\n  implies the old state %a\n"
                          T.name s.sid T.pretty d T.pretty old);
              end;
              None
          | Some d' -> begin
              (* We have changed the data *) 
              if !T.debug then 
                ignore (E.log "FF(%s): weaken data for block %d: %a\n" 
                          T.name s.sid T.pretty d');
              Some d'
          end
        with Not_found -> (* was bottom before *)
          let d' = T.computeFirstPredecessor s d in 
          if !T.debug then 
            ignore (E.log "FF(%s): set data for block %d: %a\n" 
                      T.name s.sid T.pretty d');
          Some d'
      in
      E.popContext ();
      match newdata with 
        None -> ()
      | Some d' -> 
          IH.replace T.stmtStartData s.sid d';
          if T.filterStmt s && 
            not (Queue.fold (fun exists s' -> exists || s'.sid = s.sid)
                            false
                            worklist) then 
            Queue.add s worklist

    (** Process a statement *)
    let processStmt (s: stmt) : unit = 
      if !T.debug then 
        ignore (E.log "FF(%s).stmt %a\n" T.name d_stmt s);

      (* It must be the case that the block has some data *)
      let init: T.t = 
         try T.copy (IH.find T.stmtStartData s.sid) 
         with Not_found -> 
            E.s (E.bug "FF(%s): processing block without data" T.name)
      in

      (** See what the custom says *)
      currentLoc := get_stmtLoc s.skind;
      match T.doStmt s init with 
        Done init' -> init'
      | (Default | Post _) as act -> begin
          (* Do the instructions in order *)
          let handleInstruction (s: T.t) (i: instr) : T.t = 
            currentLoc := get_instrLoc i;
            
            (* Now handle the instruction itself *)
            let s' = 
              let action = T.doInstr i s in 
              match action with 
               | Done s' -> s'
               | Default -> s (* do nothing *)
               | Post f -> f s
            in
            s'
          in

          let after: T.t = 
            match s.skind with 
              Instr il -> 
                (* Handle instructions starting with the first one *)
                List.fold_left handleInstruction init il

            | Goto _ | Break _ | Continue _ | If _ 
            | TryExcept _ | TryFinally _ 
            | Switch _ | Loop _ | Return _ | Block _ -> init
          in
          currentLoc := get_stmtLoc s.skind;
                
          (* Reach the successors *)
          List.iter (fun s' -> reachedStatement s' after) s.succs;


          match act with 
            Post f -> f ()
          | _ -> ()

      end




          (** Compute the data flow. Must have the CFG initialized *)
    let compute (sources: stmt list) = 
      Queue.clear worklist;
      List.iter (fun s -> Queue.add s worklist) sources;

      (** All initial stmts must have non-bottom data *)
      List.iter (fun s -> 
         if not (IH.mem T.stmtStartData s.sid) then 
           E.s (E.error "FF(%s): initial stmt %d does not have data"
                  T.name s.sid))
         sources;
      if !T.debug then
        ignore (E.log "\nFF(%s): processing\n"
                  T.name); 
      let rec fixedpoint () = 
        if !T.debug && not (Queue.is_empty worklist) then 
          ignore (E.log "FF(%s): worklist= %a\n" 
                    T.name
                    (docList (fun s -> num s.sid)) 
                    (List.rev
                       (Queue.fold (fun acc s -> s :: acc) [] worklist)));
          let s = Queue.take worklist in 
          processStmt s;
          fixedpoint ()
      in
      try 
        fixedpoint ()
      with Queue.Empty -> 
        if !T.debug then 
          ignore (E.log "FF(%s): done\n\n" T.name)
          
  end



(******************************************************************
 **********
 **********         BACKWARDS 
 **********
 ********************************************************************)
module type BackwardsTransfer = sig
  val name: string (* For debugging purposes, the name of the analysis *)

  val debug: bool ref (** Whether to turn on debugging *)

  type t  (** The type of the data we compute for each block start. In many 
           * presentations of backwards data flow analysis we maintain the 
           * data at the block end. This is not easy to do with JVML because 
           * a block has many exceptional ends. So we maintain the data for 
           * the statement start. *)

  val pretty: unit -> t -> Pretty.doc (** Pretty-print the state *)

  val stmtStartData: t Inthash.t
  (** For each block id, the data at the start. This data structure must be 
   * initialized with the initial data for each block *)

  val combineStmtStartData: Cil.stmt -> old:t -> t -> t option
  (** When the analysis reaches the start of a block, combine the old data 
   * with the one we have just computed. Return None if the combination is 
   * the same as the old data, otherwise return the combination. In the 
   * latter case, the predecessors of the statement are put on the working 
   * list. *)


  val combineSuccessors: t -> t -> t
  (** Take the data from two successors and combine it *)


  val doStmt: Cil.stmt -> t action
  (** The (backwards) transfer function for a branch. The {!Jvm.currentPc} is 
   * set before calling this. If it returns None, then we have some default 
   * handling. Otherwise, the returned data is the data before the branch 
   * (not considering the exception handlers) *)

  val doInstr: Cil.instr -> t -> t action
  (** The (backwards) transfer function for an instruction. The 
   * {!Jvm.currentPc} is set before calling this. If it returns None, then we 
   * have some default handling. Otherwise, the returned data is the data 
   * before the branch (not considering the exception handlers) *)

  val filterStmt: Cil.stmt -> Cil.stmt -> bool
  (** Whether to put this predecessor block in the worklist. We give the 
   * predecessor and the block whose predecessor we are (and whose data has 
   * changed)  *)
  
end

module BackwardsDataFlow = 
  functor (T : BackwardsTransfer) -> 
  struct

    let getStmtStartData (s: stmt) : T.t = 
      try IH.find T.stmtStartData s.sid
      with Not_found -> 
        E.s (E.bug "BF(%s): stmtStartData is not initialized for %d"
               T.name s.sid)

    (** Process a statement and return true if the set of live return 
     * addresses on its entry has changed. *)
    let processStmt (s: stmt) : bool = 
      if !T.debug then 
        ignore (E.log "BF(%s).stmt %a\n" T.name d_stmt s);


      (* Find the state before the branch *)
      currentLoc := get_stmtLoc s.skind;
      let d: T.t = 
        match T.doStmt s with 
           Done d -> d
         | (Default | Post _) as action -> begin
             (* Do the default one. Combine the successors *)
             let res = 
               match s.succs with 
                 [] -> E.s (E.bug "You must doStmt for the statements with no successors")
               | fst :: rest -> 
                   List.fold_left (fun acc succ -> 
                     T.combineSuccessors acc (getStmtStartData succ))
                     (getStmtStartData fst)
                     rest
             in
             (* Now do the instructions *)
             let res' = 
               match s.skind with 
                 Instr il -> 
                   (* Now scan the instructions in reverse order. This may 
                    * Stack_overflow on very long blocks ! *)
                   let handleInstruction (i: instr) (s: T.t) : T.t = 
                     currentLoc := get_instrLoc i;
                     (* First handle the instruction itself *)
                     let action = T.doInstr i s in 
                     match action with 
                     | Done s' -> s'
                     | Default -> s (* do nothing *)
                     | Post f -> f s
                   in
                   (* Handle instructions starting with the last one *)
                   List.fold_right handleInstruction il res

               | _ -> res
             in
             match action with 
               Post f -> f res'
             | _ -> res'
         end
      in

      (* See if the state has changed. The only changes are that it may grow.*)
      let s0 = getStmtStartData s in 

      match T.combineStmtStartData s ~old:s0 d with 
        None -> (* The old data is good enough *)
          false

      | Some d' -> 
          (* We have changed the data *) 
          if !T.debug then 
            ignore (E.log "BF(%s): set data for block %d: %a\n" 
                      T.name s.sid T.pretty d');
          IH.replace T.stmtStartData s.sid d';
          true


          (** Compute the data flow. Must have the CFG initialized *)
    let compute (sinks: stmt list) = 
      let worklist: Cil.stmt Queue.t = Queue.create () in
      List.iter (fun s -> Queue.add s worklist) sinks;
      if !T.debug && not (Queue.is_empty worklist) then
        ignore (E.log "\nBF(%s): processing\n" 
                  T.name); 
      let rec fixedpoint () = 
        if !T.debug &&  not (Queue.is_empty worklist) then 
          ignore (E.log "BF(%s): worklist= %a\n" 
                    T.name
                    (docList (fun s -> num s.sid)) 
                    (List.rev
                       (Queue.fold (fun acc s -> s :: acc) [] worklist)));
        try 
          let s = Queue.take worklist in 
          let changes = processStmt s in 
          if changes then begin
            (* We must add all predecessors of block b, only if not already 
             * in and if the filter accepts them. *)
            List.iter 
              (fun p ->
                if not (Queue.fold (fun exists s' -> exists || p.sid = s'.sid) 
                          false worklist) &&
                  T.filterStmt p s then 
                  Queue.add p worklist)
              s.preds;
          end;
          fixedpoint ();

        with Queue.Empty -> 
          if !T.debug then 
            ignore (E.log "BF(%s): done\n\n" T.name)
      in
      fixedpoint ();
          
  end


