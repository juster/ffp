class type scanner = object
  method tok : char -> bool
  method expr : Ffp.expr
end

let scanatom () = object (self)
  val b = Buffer.create 16

  method tok ch =
    match ch with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> Buffer.add_char b ch; true
    | _ -> false

  method expr =
    match Buffer.contents b with
    | "" -> raise Not_found
    | s -> Ffp.Atoms.of_string s
end

let scanbot () = object
  val mutable fnd = false

  method tok ch =
    fnd <- ch = '_'; fnd

  method expr =
    if fnd then Ffp.Bottom else raise Not_found
end

let scanobj () = object (self)
  val scans = ref [scanbot (); scanatom ()]

  method tok ch =
    match !scans with
    | [] -> false
    | hd :: tl ->
      if hd#tok ch then (
        true
      ) else (
        try ignore (hd#expr); false
        with Not_found -> scans := tl; self#tok ch
      )

  method expr =
    match !scans with
    | [] -> raise Not_found
    | hd :: _ -> hd#expr
end
