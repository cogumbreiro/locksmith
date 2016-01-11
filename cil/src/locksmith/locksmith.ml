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
module Lprof = Lockprofile

(*****************************************************************************)

let do_graph_out = ref false
let do_cfgraph_out = ref false
let debug = ref false

(* subversion will substitute this *)
let version = "$Rev: 1737 $"

let print_version () =
  output_string stdout ("LockSmith build #"^version)

let feature : featureDescr = {
  fd_name = "locksmith";
  fd_enabled = Cilutil.locksmith;
  fd_description = "Locksmith";

  fd_extraopt =
    [
      "--locksmith-version",
         Arg.Unit(print_version),
         "Print locksmith version and exit.";

      "--debug-locksmith",
         Arg.Set(debug),
         "Print locksmith profiling information after each phase.";

      "--save-graph",
         Arg.Set(do_graph_out),
         "Write constraints in \"graph.dot\".";

      "--save-control-flow-graph",
         Arg.Set(do_cfgraph_out),
         "Write constraints in \"cf-graph.dot\".";
    ]
    @ Locktype.options
    @ Uniqueness.options
    @ Semiunification.options
    @ Locksettings.options
    @ Livevars.options
    @ Shared.options
    @ Lockstate.options
    @ Correlation.options
    @ Controlflow.options
    @ Bansheemlifc.options
    @ Labelflow.options
    @ Lockpick.options
    ;
  fd_doit =
  (function (f: file) -> begin
    ignore(E.log
      "\n************************* STARTING *************************\n\n");
    if !debug then ignore(E.log "type program...\n");
    let cstart = Lprof.starttime () in
    Locktype.generate_constraints f;
    Labelflow.done_adding_instantiations ();
    let ctime = Lprof.endtime cstart in
    if !debug then ignore(E.log "typing done: %s " (Lprof.to_string ctime));
    if !debug then Lprof.print_mem_info ();
    if !debug then ignore(E.log "\n");

    if !do_cfgraph_out then begin
      Dotpretty.init_file "cf-graph.dot" "control flow graph";
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;

    if !do_graph_out then begin
      Dotpretty.init_file "graph-begin.dot" "initial constraints";
      Labelflow.print_graph !Dotpretty.outf;
      Semiunification.print_graph !Dotpretty.outf;
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;

    if !debug then ignore(E.log "find shared\n");
    let shstart = Lprof.starttime() in
    Shared.solve (Locktype.get_global_var_rhos ());
    let shtime = Lprof.endtime shstart in
    if !debug then ignore(E.log "shared done: %s " (Lprof.to_string shtime));
    if !debug then Lprof.print_mem_info ();
    if !debug then ignore(E.log "\n");

    if !Lockpick.do_lockpick then Lockpick.doit()
    else begin

      if !debug then ignore(E.log "solve semi-unification (lock linearity)...\n");
      (*if !do_graph_out then begin
        Dotpretty.init_file "su.dot";
        Semiunification.print_graph !Dotpretty.outf;
        Dotpretty.close_file ();
      end;
      *)
      let sustart = Lprof.starttime () in
      Semiunification.solve ();
      let sutime = Lprof.endtime sustart in
      (*
      if !do_graph_out then begin
        Dotpretty.init_file "sus.dot";
        Semiunification.print_graph !Dotpretty.outf;
        Dotpretty.close_file ();
      end;
      *)
      if !debug then ignore(E.log "linearity done: %s " (Lprof.to_string sutime));
      if !debug then Lprof.print_mem_info ();
      if !debug then ignore(E.log "\n");

      if !debug then ignore(E.log "compute lock state...\n");
      let lsstart = Lprof.starttime () in
      Lockstate.solve ();
      let lstime = Lprof.endtime lsstart in
      if !debug then ignore(E.log "state done: %s " (Lprof.to_string lstime));
      if !debug then Lprof.print_mem_info ();
      if !debug then ignore(E.log "\n");

      if !debug then ignore(E.log "find guarded-by...\n");
      let gbstart = Lprof.starttime () in
      Correlation.solve ();
      let gbtime = Lprof.endtime gbstart in
      if !debug then ignore(E.log "guardedby done: %s " (Lprof.to_string gbtime));
      if !debug then Lprof.print_mem_info ();
      if !debug then ignore(E.log "\n");

      if !debug then ignore(E.log "check escapes()...\n");
      let escstart = Lprof.starttime () in
      Semiunification.check ();
      let esctime = Lprof.endtime escstart in
      if !debug then ignore(E.log "escapes done in %s " (Lprof.to_string esctime));
      if !debug then Lprof.print_mem_info ();
      if !debug then ignore(E.log "\n");


      if !debug then ignore(E.log "check for races...\n");
      let rcstart = Lprof.starttime () in
      Correlation.check_races ();
      let rctime = Lprof.endtime rcstart in
      if !do_graph_out then begin
        Dotpretty.init_file "graph.dot" "solved constraints";
        Labelflow.print_graph !Dotpretty.outf;
        Semiunification.print_graph !Dotpretty.outf;
        Lockstate.print_graph !Dotpretty.outf;
        Dotpretty.close_file ();
      end;
      if !debug then ignore(E.log "races done: %s " (Lprof.to_string rctime));
      if !debug then Lprof.print_mem_info ();
      if !debug then ignore(E.log "\n");
    end;

    ignore(E.log
      "*************************** DONE ***************************\n\n");
    ignore(E.log "LockSmith run for: %f seconds\n\n" (Sys.time()));
  end);
  fd_post_check = true;
}
