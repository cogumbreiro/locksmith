open Pretty
module U = Uref

type label_name =
  | Const      of string
  | AddrOf     of label_name
  | List       of label_name list U.uref
  | Field      of label_name * string
  | Deref      of label_name

let rec d_label_name () l =
  match l with 
  | Const s -> text s
  | AddrOf l -> dprintf "&%a" d_label_name l
  | List llu -> dprintf "(%a)" (d_list ", " d_label_name) (U.deref llu)
  | Field (Deref l,f) -> dprintf "%a->%s" d_label_name l f
  | Field (l, f) -> dprintf "%a.%s" d_label_name l f
  | Deref l -> dprintf "*%a" d_label_name l
