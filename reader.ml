open Ffp

type reader = { buffer:string; mutable next:char option; n:int; mutable i:int }
exception BadSyntax of int

let badsyn r =
  match r.next with
  | None -> raise (BadSyntax r.i)
  | Some _ -> raise (BadSyntax (r.i-1))

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

let nextchar r =
  match r.next with
  | None -> badsyn r
  | Some ch -> ch

let atomchar ch =
  match ch with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false

let skip r ch =
  if (nextchar r) = ch then pump r else badsyn r

let rec skipws r =
  match r.next with
  | None -> ()
  | Some ' ' -> pump r; skipws r
  | Some _ -> ()

let rec expr r =
  skipws r;
  match nextchar r with
  | '<' -> sequence r
  | ch ->
    if atomchar ch then (
      let a = atom r (Buffer.create 16) in
      match r.next with
      | Some ':' -> application r a
      | None | Some _ -> a
    ) else badsyn r

and sequence r =
  skip r '<';
  skipws r;
  if nextchar r = '>' then (
    pump r; Sequence []
  ) else (
    let s = seq r [] in
    skip r '>';
    Sequence s
  )

and seq r lst =
  let lst = (expr r) :: lst in
  skipws r;
  if nextchar r = ','
  then (pump r; seq r lst)
  else List.rev lst

and atom r buf =
  let newatom buf =
    Atoms.of_string (Buffer.contents buf)
  in
  match r.next with
  | None -> newatom buf
  | Some ch ->
    if not (atomchar ch) then newatom buf
    else (
      pump r;
      atom r (Buffer.add_char buf ch; buf)
    )

and application r a =
  skip r ':';
  App (a, expr r)
