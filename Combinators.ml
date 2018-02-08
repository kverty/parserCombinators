open Lazy

let of_chars chars =
  let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf

let of_string s =
  let n = String.length s in
  let rec loop i =
    if i = n then [] else s.[i] :: loop (i + 1)
  in
  loop 0

module Errors:
  sig
    type error =
    | Delete of char * int
    | Replace of char * int

    type t = error list

    val addError : error -> t -> t
    val empty    : t
    val length   : t -> int
    val show     : t -> string
    val equal    : t -> t -> bool

  end =
  struct
    type error =
    | Delete of char * int
    | Replace of char * int

    let equalErr err1 err2 = match err1, err2 with
                             | Delete (c1, pos1), Delete (c2, pos2) when c1 = c2 && pos1 = pos2 -> true
			     | Replace (c1, pos1), Replace (c2, pos2) when c1 = c2 && pos1 = pos2 -> true
			     | _, _ -> false

    type t = error list

    let addError err errs = err :: errs
    let empty             = []
    let length            = List.length
    let showError     err = match err with
                            | Delete (c, pos) -> Printf.sprintf "#char %c was deleted on pos %d#\n" c (pos + 1)
			    | Replace (c, pos) -> Printf.sprintf "#char %c was replaced with on pos %d#\n" c (pos + 1)
    let show         errs = List.fold_left (fun acc err -> acc ^ showError err) (Printf.sprintf "%d errors:\n" (List.length errs)) errs
    let equal errs1 errs2 = try List.fold_right2 (fun err1 err2 acc -> acc && (equalErr err1 err2)) errs1 errs2 true
                            with _ -> false
  end

type ('b, 'stream) result = ('b * 'stream) list

class stream (s : char list) =
  object (self : 'self)

    val p = 0
    val errors = Errors.empty

    method errors    = errors
    method pos       = p
    method str       = of_chars s
    method chrs      = s
    method rest      =
      let rec f l p' = if (p' = 0) then l else f (List.tl l) (p' - 1)
      in of_chars (f s p)

    method correctErrors =
      let rec cycle str offset = function
      | []                            -> str
      | Errors.Replace (c, pos) :: etc -> cycle str offset etc
      | Errors.Delete (c, pos) :: etc -> cycle str (offset + 1) etc
      in cycle (of_chars s) 0 errors

    method equal : stream -> bool =
      fun s' -> (s = s' # chrs) && (p = s' # pos) && (Errors.equal errors (s' # errors))

    method look : 'b . char -> (char -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun c k ->
      try
        if c = List.nth s p
        then k c {< p = p + 1 >}
        else begin
          ({< p = p + 1; errors = Errors.addError (Errors.Delete (List.nth s p, p)) errors >} # look c k) @
	  (k c {< p = p + 1; errors =  Errors.addError (Errors.Replace (c, p)) errors>})
	end
      with _ -> []

    method getEOF : 'b . (unit -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        if p = List.length s
        then k () self
        else []
  end

module K :
  sig
    type ('a, 'b) t = 'a -> stream -> ('b, stream) result
    type ks

    val singleton : ('a, 'b) t -> ks
    val add       : ('a, 'b) t -> ks -> ks
    val fold      : (('a, 'b) t -> ('b, stream) result -> ('b, stream) result) -> ks -> ('b, stream) result -> ('b, stream) result
    val empty     : ks
    val length    : ks -> int

  end =
  struct

    type ('a, 'b) t = 'a -> stream -> ('b, stream) result

    module Ks = Set.Make (
      struct
	type t = Obj.t

	let compare x y = (Pervasives.compare : int -> int -> int) (Obj.magic x) (Obj.magic y)
      end
    )
    type ks = Ks.t

    let singleton k         = Ks.add (Obj.repr k) Ks.empty
    let add       k ks      = Ks.add (Obj.repr k) ks
    let fold      f ks acc  = Ks.fold (fun k acc -> f (Obj.magic k) acc) ks acc
    let empty               = Ks.empty
    let length      ks      = Ks.cardinal ks
  end

type ('a, 'b) k       = ('a, 'b) K.t
type ('a, 'b) parser  = ('a, 'b) k -> stream -> ('b, stream) result
type ('a, 'b) parser' = ('a, 'b) k ->           ('b, stream) result

let success : 'a -> ('a, 'b) parser = fun a k s -> k a s
let failure :       ('a, 'b) parser = fun   k s -> []

let empty = fun k -> success [] k

let seq : ('a, 'b) parser -> ('a -> ('c, 'b) parser) -> ('c, 'b) parser =
  fun x y k -> x (fun a -> y a k)

let (|>) = seq

let memoresult : ('a, 'b) parser' -> ('a, 'b) parser' =
  fun p ->
    let ss : (stream * 'a) list ref = ref      [] in
    let ks :               K.ks ref = ref K.empty in
    fun k ->
      if K.length !ks = 0
      then (
        ks := K.singleton k;
        p (fun a s ->
             match List.find_all (fun (s', a') -> (a = a') && (s # equal s')) !ss with
	     | [] -> (ss := (s, a) :: !ss;
	              K.fold (fun k acc -> acc @ (k a s)) !ks [])
             |  _ -> []
          ))
      else (ks := K.add k !ks;
	    List.flatten (List.map (fun (s, a) -> (k a s)) !ss ))

let memo : ('a, 'b) parser -> ('a, 'b) parser =
  fun f ->
    let table : (stream, ('a, 'b) parser') Hashtbl.t = Hashtbl.create 16 in
    fun k s ->
      try Option.get (Hashtbl.fold (fun s' p' acc -> match acc with
                                                     | Some _                   -> acc
					             | None when (s # equal s') -> Some p'
					             | _                        -> None
				   ) table None) k
      with _ ->
        let r = memoresult @@ (fun k -> f k s) in
        Hashtbl.add table s r; r k

let alt : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser =
  fun x y -> memo (fun k s -> (x k s) @ (y k s))

let (<|>) = alt

let fix : (('a, 'b) parser -> ('a, 'b) parser) -> ('a, 'b) parser =
  fun f -> let rec p = lazy (f (fun t -> force p t)) in force p

let lift : ('a -> 'b) -> ('b, stream) result -> ('b, stream) result =
  fun f rs -> List.map (fun (a, s) -> (f a, s)) rs

let map : ('a -> 'c) -> ('a, 'b) parser -> ('c, 'b) parser =
  fun f p -> p |> (fun a -> success (f a))

let rec manyFold : (('a -> 'c -> 'c) -> 'c -> ('a, 'b) parser -> ('c, 'b) parser) =
  fun f init p -> (empty |> fun _ -> success init) <|>
                  (p                 |> (fun xp  ->
	           manyFold f init p |> (fun xps ->
		   success (f xp xps))))
(*
let many : ('a, 'b) parser -> ('a, 'b) parser =
  fun p ->
    (manyFold (fun acc x -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<*>) = many
*)
let someFold : (('a -> 'c -> 'c) -> 'c -> ('a, 'b) parser -> ('c, 'b) parser) =
  fun f init p -> p                 |> (fun xp  ->
	          manyFold f init p |> (fun xps ->
	          success (f xp xps)))
(*
let some ('a, 'b) parser -> ('a, 'b) parser =
  fun p ->
    (someFold (fun acc x -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<+>) = some
*)

let bestResult results =
try Some (List.fold_left (fun (r1, s1) (r2, s2) -> if Errors.length s1 # errors < Errors.length s2 # errors then (r1, s1) else (r2, s2))
                         (List.hd results)
		          results)
with _ -> None

let printBestResult parser testNum testNumNum resType input =
  match bestResult @@ parser (new stream @@ of_string @@ input) with
  | Some (res, s) -> Printf.printf ("Test%s.%s: %s parsed with result = " ^^ resType ^^ " and %s\n") testNum testNumNum input res (Errors.show s # errors)
  | None          -> Printf.printf "Test%s.%s crushed\n" testNum testNumNum

let printAllResults parser testNum testNumNum resType input =
  List.iter (fun (res, s) -> Printf.printf ("Test%s.%s: %s parsed with result = " ^^ resType ^^ " and %s\n") testNum testNumNum input res (Errors.show s # errors)) (parser (new stream @@ of_string @@ input))

let printResult = printBestResult
