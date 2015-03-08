open Reader
open Ffp

(*
term -> expr | cond |> compose
cond -> expr `-' `>' term `|' term
compose -> expr `|' `>' expr
expr -> object | subexpr | app | compose | every | select

object -> atom | seq
atom -> {atomch}
atomch -> `a'..`z' | `A'..`Z' | `_' | `?' | `#'
seq -> `<' `>' | `<' seqlist `>'
seqlist -> object `,' seqlist | object

subexpr -> `(' term `)'
app -> atom `:' expr

every -> '*' atom
select -> {selch}
selch -> '0'..'9'
*)

let selch r =
  match r.next with
  | Some ('0'..'9') -> true
  | _ -> false

let nextnum r =
  let x = (Char.code (nextchar r)) - (Char.code '0') in
  assert ((x >= 0) && (x <= 9)); x

let select r n =
  let i = ref n in
  while selch r do
    i := (!i * 10) + (nextnum r)
  done;
  Sequence [ Atom Forms.select; Atom !i ]

let rec term r =
  let e = expr r in
  skipws r;
  if skipstr r "->" then
    cond r e
  else if (skipws r; skipstr r "|>") then
    (skipws r; Comp (expr r, e))
  else
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
  else if skip r '*' then
    every r
  else if selch r then
    select r (nextnum r)
  else match obj r with
    | Atom _ as a ->
      begin
        if skip r ':' then
          App (a, expr r)
        else
          a
      end
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
  while atomnext r do
    Buffer.add_char b (nextchar r)
  done;
  match Buffer.contents b with
  | "" -> badsyn r
  | s ->
    let x = try Atoms.find s with Not_found -> Atoms.add s in
    Atom x

and subexp r =
  skipws r;
  let e = term r in
  skipws r;
  next r ')';
  e

and every r =
  Sequence [ Atom Forms.every; atom r ]

let read = term
