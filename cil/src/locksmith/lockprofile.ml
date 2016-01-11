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
external print_memusage : unit -> unit = "print_usage"
module E = Errormsg

type t = float
type timerange = float

let starttime () : t =
  Sys.time ()

let endtime (s: t) : timerange =
  (Sys.time()) -. s

let to_string (r: timerange ) : string =
  (string_of_float r) ^ "sec"

let print_mem_info () : unit = begin
  print_memusage ();
  let s: Gc.stat = Gc.stat () in
  ignore(E.log "/%d words" s.Gc.heap_words);
end

let last_timestamp = ref 0

let abs_time_to_string t =
  let s = t mod 60 in
  let m = (t mod 3600) / 60 in
  let h = t / 3600 in
  Printf.sprintf "%02d:%02d:%02d" h m s

let timestamp () =
  let t = int_of_float (Sys.time ()) in
  let d = t - (!last_timestamp) in
  begin
    last_timestamp := t;
    "(" ^ (abs_time_to_string t) ^ " [+" ^ (abs_time_to_string d) ^ "])"
  end
