open Reader
open Ffp

(*
term -> expr | cond
cond -> expr `-' `>' expr `|' expr
expr -> atom | seq | subexpr
atom -> [a-zA-Z_*?#]+
seq -> `<' `>' | `<' seqlist `>'
seqlist -> expr `,' seqlist | expr
subexpr -> `(' term `)'
*)

let rec term r =
  let e = expr r in
  skipws r;
  match r.next with
  | Some '-' ->
    begin
      pump r;
      match r.next with
      | Some '>' ->
        pump r;
        cond r e
      | _ ->
        backup r;
      e
    end
  | _ ->
    e

and cond r c =
  skipws r;
  let t = expr r in
  skipws r;
  next r '|';
  skipws r;
  let f = expr r in
  Cond (c, t, f)

and expr r =
  skipws r;
  if skip r '<' then
    sequence r
  else if atomnext r then
    let a = atom r in
    if skip r ':' then
      App (a, expr r)
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
  let s = Buffer.contents b in
  let x = try Atoms.find alist s with Not_found -> Atoms.add alist s in
  Atom x

let read = term
