open Reader
open Ffp

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
