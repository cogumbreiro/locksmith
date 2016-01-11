type label_name =
  | Const      of string
  | AddrOf     of label_name
  | List       of label_name list Uref.uref
  | Field      of label_name * string
  | Deref      of label_name

val d_label_name : unit -> label_name -> Pretty.doc
