type expr =
  Bottom | Atom of int | Sequence of expr list |
  App of expr * expr |
  Bytes of string |
  Cond of expr * expr * expr |
  Comp of expr * expr

module Atoms = struct
  let null = 0
  let truth = 1
  let fallicy = 2
  let default = 3

  let alist = ref [ "?", null; "T", truth; "F", fallicy; "#", default ]
  let n = ref 4

  let next () =
    let i = !n in
    incr n; i

  let add s =
    let a = next () in
    alist := (s, a) :: !alist;
    a

  let find s =
    List.assoc s !alist

  let to_string x =
    fst (List.find (fun (s, y) -> x = y) !alist)
end

(* Bottom preserving application of a function to every sub-expression of a
   sequence. *)

let mapseq f l =
  let rec mapseq' f l l' =
    match l with
    | [] -> Sequence (List.rev l')
    | Bottom :: _ -> Bottom
    | hd :: tl ->
      match f hd with
      | Bottom -> Bottom
      | x -> mapseq' f tl (x :: l')
  in
  mapseq' f l []

(* Primitive functions. Map objects to objects like FP functions. *)

module Prims = struct
  let nullat = Atom Atoms.null
  let truth = Atom Atoms.truth
  let fallicy = Atom Atoms.fallicy

  let boolean b =
    if b then truth else fallicy

  let selector n = function
    | Sequence l ->
      begin
        try List.nth l (n-1)
        with Failure _ -> Bottom
      end
    | Bytes s ->
      begin
        try
          let s = String.make 1 (String.get s (n-1)) in
          Atom (Atoms.add s)
        with Invalid_argument _ -> Bottom
      end
    | _ -> Bottom
  
  let tail = function
    | Sequence (_ :: []) -> nullat
    | Sequence (hd :: tl) -> Sequence tl
    | Bytes "" -> nullat
    | Bytes s ->
      begin
        try
          let len = (String.length s) - 1 in
          let s' = String.sub s 1 len in
          Bytes s'
        with Invalid_argument _ -> assert false
      end
    | Atom _ as a when a = nullat -> a
    | _ -> Bottom
  
  let identity x = x
  
  let atom = function
    | Atom _ -> truth
    | Bottom -> Bottom
    | _ -> fallicy
  
  let equals = function
    | Sequence [Atom x; Atom y] -> boolean (x = y)
    | Sequence [Bytes s; Bytes s'] -> boolean (s = s')
    | _ ->
      Bottom

  let null = function
    | Bottom -> Bottom
    | Atom _ as x -> boolean (x = nullat)
    | Sequence [] -> truth
    | _ -> fallicy

  let reverse x =
    let rec revstr s =
      let n = String.length s in
      let s' = String.create n in
      for i = 1 to n do
        s'.[n-i] <- s.[i-1]
      done; s'
    in
    match x with
    | Sequence [] -> nullat
    | Sequence l -> Sequence (List.rev l)
    | Bytes "" -> nullat
    | Bytes s -> Bytes (revstr s)
    | Atom _ as a when a = nullat -> a
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
    add "apply" apply;
end

(* Function forms. *)

module Forms = struct
  let every = Atoms.next ()

  let flist = ref []

  let add i expr =
    flist := (i, expr) :: !flist

  let find i =
    List.assq i !flist
end

(* The representation function associates objects with the functions they
   represent. The result is a closure which take an object argument and returns
   an expression. *)

let rec repr = function
  | Atom x ->
    begin
      try Prims.find x with Not_found -> (fun _ -> Bottom)
    end
  | Sequence (x :: _) as s ->
    (fun y -> (repr x) (Sequence [s; y]))
  | Bytes _ | Bottom ->
    (fun _ -> Bottom)
  | Sequence [] | App (_, _) | Cond (_, _, _) | Comp (_, _) ->
    assert false

(* The meaning function determines the value of an FFP expression, which is
   always an object. *)

let rec meaning = function
  | App (Comp (f, g), x) -> meaning (App (f, App (g, x)))
  | App (Cond (c, t, f), x) ->
    begin
      match meaning (App (c, x)) with
      | Atom i when i = Atoms.truth -> meaning (App (t, x))
      | Atom i when i = Atoms.fallicy -> meaning (App (f, x))
      | _ -> Bottom
    end
  | App (x, y) -> meaning ((repr (meaning x)) (meaning y))
  | Comp (_, _) | Cond (_, _, _) -> assert false
  | Sequence l -> mapseq meaning l
  | (Atom _ | Bottom | Bytes _) as x -> x
