open Ffp

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

let rec expr r =
  skipws r;
  if skip r '<' then
    sequence r
  else if atomnext r then
    let a = atom r in
    if skip r ':' then
      application r a
    else
      a
  else
    badsyn r

and sequence r =
  skipws r;
  if skip r '>' then
    Sequence []
  else
    let s = seq r [] in
    next r '>';
    Sequence s

and seq r lst =
  let lst = (expr r) :: lst in
  skipws r;
  if skip r ',' then
    seq r lst
  else
    List.rev lst

and atom r =
  let b = Buffer.create 16 in
  Buffer.add_char b (nextchar r); pump r; (* must be at least 1 char *)
  while atomnext r do
    Buffer.add_char b (nextchar r); pump r;
  done;
  Atoms.of_string (Buffer.contents b)

and application r a =
  App (a, expr r)
