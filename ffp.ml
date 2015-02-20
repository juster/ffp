type expr =
  Bottom | Atom of int | Sequence of expr list |
  App of expr * expr |
  Bytes of string

module Atoms = struct
  let alist = ref []
  let n = ref 0

  let add s =
    let a = Atom !n in
    alist := (s, a) :: !alist; incr n; a

  let find s =
    List.assoc s !alist

  let _ =
    ignore (List.map add [ "_"; "T"; "F"; "?" ])
end

(* Primitive functions. Map objects to objects like FP functions. *)

module Prims = struct
  let selector n = function
    | Sequence l ->
      begin
        try List.nth l (n-1)
        with Failure _ -> Bottom
      end
    | Bytes s ->
      begin
        try
          let c = (String.get s (n-1)) in
          Atoms.add (String.make 1 c)
        with Invalid_argument _ -> Bottom
      end
    | _ -> Bottom
  
  let tail = function
    | Sequence (_ :: []) -> Atoms.find "_"
    | Sequence (hd :: tl) -> Sequence tl
    | Bytes "" -> Atoms.find "_"
    | Bytes s ->
      begin
        try
          let len = (String.length s) - 1 in
          let s' = String.sub s 1 len in
          Bytes s'
        with Invalid_argument _ -> assert false
      end
    | _ -> Bottom
  
  let identity x = x
  
  let atom = function
    | Atom _ -> Atoms.find "T"
    | Bottom -> Bottom
    | _ -> Atoms.find "F"
  
  let equals = function
    | Sequence [Atom _ as y; Atom _ as z] ->
      Atoms.find (if y == z then "T" else "F")
    | Sequence [Bytes s; Bytes s'] ->
      Atoms.find (if s = s' then "T" else "F")
    | _ ->
      Bottom

  let null = function
    | Bottom -> Bottom
    | Atom _ as a -> if a == Atoms.find "_" then Atoms.find "T" else Atoms.find "F"
    | Sequence [] -> Atoms.find "T"
    | _ -> Atoms.find "F"

  let reverse x =
    let rec revstr s =
      let n = String.length s in
      let s' = String.create n in
      for i = 1 to n do
        s'.[n-i] <- s.[i-1]
      done; s'
    in
    match x with
    | Sequence [] -> Atoms.find "_"
    | Sequence l -> Sequence (List.rev l)
    | Bytes "" -> Atoms.find "_"
    | Bytes s -> Bytes (revstr s)
    | _ -> Bottom

  let apply = function
    | Sequence [x; y] -> App (x, y)
    | _ -> Bottom

  let const c = function Bottom -> Bottom | _ -> c

  let plist = ref []

  let add s f =
    plist := (Atoms.add s, f) :: !plist

  let find a =
    List.assq a !plist

  let _ =
    add "1" (selector 1);
    add "tail" tail;
    add "id" identity;
    add "atom" atom;
    add "eq" equals;
    add "null" null;
    add "rev" reverse;
    add "apply" apply
end

(* User defined functions are expressions which map expressions to expressions. *)

let defs = ref []

(* The representation function associates objects with the functions they represent.
   The result is a closure which take an object argument and returns an expression. *)

let rec repr = function
  | Atom _ as x ->
    begin
      try Prims.find x with Not_found ->
        try List.assq x !defs with Not_found ->
          (fun _ -> Bottom)
    end
  | Sequence (x :: _) as s ->
    (fun y -> (repr x) (Sequence [s; y]))
  | Bytes _ | Bottom ->
    (fun _ -> Bottom)
  | Sequence [] | App (_, _) ->
    assert false

(* Bottom preserving application of a function to every sub-expression of a sequence. *)

let rec mapseq f l l' =
  match l with
  | [] -> Sequence (List.rev l')
  | hd :: tl ->
    match f hd with
    | Bottom -> Bottom
    | x -> mapseq f tl (x :: l')

(* The meaning function determines the value of an FFP expression, which is always an object. *)

let rec meaning = function
  | App (x, y) -> meaning ((repr (meaning x)) (meaning y))
  | Sequence l -> mapseq meaning l []
  | (Atom _ | Bottom | Bytes _) as x -> x
