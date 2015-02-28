type reader = { buffer:string; mutable next:char option; n:int; mutable i:int }
exception BadSyntax of int

let badsyn r =
  match r.next with
  | None -> raise (BadSyntax r.i)
  | Some _ -> raise (BadSyntax (r.i-1))

let pump r =
  if r.i >= r.n then
    r.next <- None
  else (
    r.next <- Some r.buffer.[r.i];
    r.i <- r.i + 1
  )

let backup r =
  if r.i <= 1 then assert false
  else (r.i <- r.i - 1; r.next <- Some r.buffer.[r.i - 1])

let create s =
  let r = { buffer = s; next = None; n = String.length s; i = 0 } in
  pump r; r

(* Retrieve the next character or fail with a syntax error. *)

let nextchar r =
  match r.next with
  | None -> badsyn r
  | Some ch -> ch

let atomnext r =
  match r.next with
  | None -> false
  | Some ('a'..'z' | 'A'..'Z' | '0'..'9') -> true
  | Some _ -> false

let skip r ch =
  match r.next with
  | None -> false
  | Some ch' ->
    if ch = ch' then
      (pump r; true)
    else
      false

let rec skipws r =
  while skip r ' ' do () done

let next r ch =
  if not (skip r ch) then badsyn r else ()
