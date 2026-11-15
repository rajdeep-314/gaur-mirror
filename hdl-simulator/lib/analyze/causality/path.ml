type t =
  { root    : string
  ; fields  : string list
  ; is_next : bool
  }

let make ?(fields = []) ?(is_next = false) root =
  { root; fields; is_next }

let var name = { root = name; fields = []; is_next = false }
let port name fields = { root = name; fields; is_next = false }
let state name = { root = name; fields = []; is_next = false }
let next_state name = { root = name; fields = []; is_next = true }

let add_field p fld = { p with fields = p.fields @ [ fld ] }
let add_fields p flds = { p with fields = p.fields @ flds }

let match_port expected_port p =
  if String.equal p.root expected_port then Some p.fields else None

let to_string p =
  let base = if p.is_next then p.root ^ "'" else p.root in
  match p.fields with
  | [] -> base
  | fs -> base ^ "." ^ String.concat "." fs

let pp ppf p = Fmt.string ppf (to_string p)

let compare (p1 : t) (p2 : t) =
  let c = String.compare p1.root p2.root in
  if c <> 0 then c
  else
    let c = Bool.compare p1.is_next p2.is_next in
    if c <> 0 then c
    else List.compare String.compare p1.fields p2.fields

let equal (p1 : t) (p2 : t) =
  String.equal p1.root p2.root
  && Bool.equal p1.is_next p2.is_next
  && List.equal String.equal p1.fields p2.fields

let hash (p : t) =
  Hashtbl.hash (p.root, p.is_next, p.fields)
