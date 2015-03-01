open Reader
open Ffp

(*
term -> expr | cond
cond -> expr `-' `>' term `|' term
expr -> object | subexpr | app

object -> atom | seq
atom -> atomch atom | atomch
atomch -> `a'..`z' | `A'..`Z' | `_' | `?' | `#'
seq -> `<' `>' | `<' seqlist `>'
seqlist -> object `,' seqlist | object

subexpr -> `(' term `)'
app -> atom `:' expr

every -> '*' expr
selector -> selch selector | selch
selch -> '0'..'9'
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
  let t = term r in
  skipws r;
  next r '|';
  skipws r;
  let f = term r in
  Cond (c, t, f)

and expr r =
  skipws r;
  if skip r '(' then
    subexp r
  else match obj r with
    | Atom _ as a when skip r ':' -> App (a, expr r)
    | e -> e

and obj r =
  if skip r '<' then
    sequence r
  else
    atom r

and sequence r =
  skipws r;
  if skip r '>' then
    Sequence []
  else
    let s = seq r [] in
    next r '>';
    Sequence s

and seq r lst =
  skipws r;
  let lst = (obj r) :: lst in
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

and subexp r =
  skipws r;
  let e = term r in
  skipws r;
  next r ')';
  e

let read = term
