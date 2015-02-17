type obj = Atom of string | Bottom | Sequence of obj list
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

(* Primitive functions. Map objects to expressions. *)

module Prims = struct
	let selector n = function
	  | Sequence l ->
	    begin
	      try List.nth l (n-1)
	      with Failure _ -> Bottom
	    end
	  | _ -> Bottom
	
	let tail = function
	  | Sequence (_ :: []) -> Atoms.null
	  | Sequence (hd :: tl) -> Sequence tl
	  | _ -> Bottom
	
	let identity x = x
	
	let atom = function
	  | Atom _ -> Atoms.truth
	  | Bottom -> Bottom
	  | _ -> Atoms.fallicy
	
	let equals = function
  	| Sequence [(Atom _ as y); (Atom _ as z)] ->
      if y == z then Atoms.truth else Atoms.fallicy
    | _ ->
      Bottom

  let null = function
    | Bottom -> Bottom
    | Atom _ as a -> if a == Atoms.null then Atoms.truth else Atoms.fallicy
    | Sequence [] -> Atoms.truth
    | _ -> Atoms.fallicy

  let reverse x =
    let o = match x with
      | Sequence [] -> Atoms.null
      | Sequence l -> Sequence (List.rev l)
      | _ -> Bottom
    in o
	
	let const c = function Bottom -> Bottom | _ -> c
end

(* Primitives are closures which map objects to objects. *)

let prims = [
 	Atoms.add "1", Prims.selector 1;
  Atoms.add "tail", Prims.tail;
  Atoms.add "id", Prims.identity;
  Atoms.add "atom", Prims.atom;
  Atoms.add "eq", Prims.equals;
  Atoms.add "null", Prims.null;
  Atoms.add "rev", Prims.reverse
]

(* User defined functions are expressions which map expressions to expressions. *)

let defs = ref []

(* The representation function associates objects with the functions they represent.
   The result is a closure which take an object argument and returns an expression. *)

let rec repr x =
	let prim f x = Obj (f x) in
  let bot = prim (Prims.const Bottom) in
  match x with
  | Bottom -> bot
  | Sequence _ -> failwith "repr was given a sequence as argument"
  | Atom _ as x ->
    try prim (List.assq x prims)
    with Not_found ->
      try apply (List.assq x !defs)
      with Not_found ->
        bot

(* The meaning function determines the value of an FFP expression, which is always an object. *)

and meaning = function
  | Obj (Atom _ as x) | Obj (Bottom as x) | Obj (Sequence _ as x) -> x
  | App (Obj (Sequence []), _) ->
    failwith "the application has an empty list as operator"
  | App (Obj (Sequence (x1 :: _)) as form, operand) ->
    let f = repr x1 in
    let x = Sequence [meaning form; meaning operand] in
    meaning (f x) (* metacomposition rule *)
  | App (operator, operand) ->
    let f = repr (meaning operator) in
		let x = meaning operand in
    meaning (f x)

and apply x y =
  App (x, Obj y)
