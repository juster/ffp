type reader = { buffer:string; mutable next:char option; n:int; mutable i:int }
exception BadSyntax of int

let pump r =
  if r.i >= r.n then (
    r.next <- None
  ) else (
    r.next <- Some r.buffer.[r.i];
    r.i <- r.i + 1
  )

let create s =
  let r = { buffer = s; next = None; n = String.length s; i = 0 } in
  pump r; r

let badsyn r =
  raise (BadSyntax (r.i-1))

let skip r ch =
  match r.next with
  | None -> badsyn r
  | Some ch' -> if ch = ch' then pump r else badsyn r

let rec skipws r =
  match r.next with
  | None -> badsyn r
  | Some ch -> if ch = ' ' || ch = '\t' then (pump r; skipws r) else ()
