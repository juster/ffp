type obj = Atom of string | Bytes of string | Bottom | Sequence of obj list
type expr = Obj of obj | App of expr * expr

module Atoms = struct
  let null = Atom "_"
  let truth = Atom "T"
  let fallicy = Atom "F"
  let default = Atom "#"
  
  let alist = ref [
    null; truth; fallicy; default
  ]

  let add s =
    let a = Atom s in
    alist := a :: !alist;
    a

  let find s =
    List.find (function Atom s' -> s = s' | _ -> false) !alist
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
          Atom (String.make 1 c)
        with Invalid_argument _ -> Bottom
      end
    | _ -> Bottom
  
  let tail = function
    | Sequence (_ :: []) -> Atoms.null
    | Sequence (hd :: tl) -> Sequence tl
    | Bytes "" -> Atoms.null
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
    | Atom _ -> Atoms.truth
    | Bottom -> Bottom
    | _ -> Atoms.fallicy
  
  let equals = function
    | Sequence [(Atom _ as y); (Atom _ as z)] ->
      if y == z then Atoms.truth else Atoms.fallicy
    | Sequence [(Bytes s); (Bytes s')] ->
      if s = s' then Atoms.truth else Atoms.fallicy
    | _ ->
      Bottom

  let null = function
    | Bottom -> Bottom
    | Atom _ as a -> if a == Atoms.null then Atoms.truth else Atoms.fallicy
    | Sequence [] -> Atoms.truth
    | _ -> Atoms.fallicy

  let reverse x =
    let rec revstr s i n =
      if n == 0 then ()
      else begin
        let m = n/2 in
        String.blit s i s m m;
        revstr s 0 m; revstr s m (n-m)
      end
    in
    let o = match x with
      | Sequence [] -> Atoms.null
      | Sequence l -> Sequence (List.rev l)
      | Bytes "" -> Atoms.null
      | Bytes s ->
        let s' = String.copy s in
        revstr s' 0 (String.length s'); Bytes s'
      | _ -> Bottom
    in o
  
  let const c = function Bottom -> Bottom | _ -> c

  let plist = ref []

  let add s f =
    plist := (Atoms.add s, f) :: !plist

  let find x =
    List.assq x !plist

  let _ =
    add "1" (selector 1);
    add "tail" tail;
    add "id" identity;
    add "atom" atom;
    add "eq" equals;
    add "null" null;
    add "rev" reverse
end

(* User defined functions are expressions which map expressions to expressions. *)

let defs = ref []

(* The representation function associates objects with the functions they represent.
   The result is a closure which take an object argument and returns an expression. *)

let repr x =
  let apply x y = App (x, Obj y) in
  let prim f x = Obj (f x) in
  let bot = prim (Prims.const Bottom) in
  match x with
  | Bytes _ | Bottom -> bot
  | Sequence _ -> assert false
  | Atom _ as x ->
    try prim (Prims.find x)
    with Not_found ->
      try apply (List.assq x !defs)
      with Not_found ->
        bot

(* The meaning function determines the value of an FFP expression, which is always an object. *)

let rec meaning = function
  | Obj x -> x
  | App (Obj (Sequence []), _) ->
    failwith "the application has an empty list as operator"
  | App (Obj (Sequence (x1 :: _) as s), operand) ->
    let f = repr x1 in
    let x = Sequence [s; meaning operand] in
    meaning (f x) (* metacomposition rule *)
  | App (operator, operand) ->
    let f = repr (meaning operator) in
    let x = meaning operand in
    meaning (f x)
