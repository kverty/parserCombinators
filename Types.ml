open Errors

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
      | Errors.Delete (c, pos)  :: etc -> cycle str (offset + 1) etc
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

type 'b execResult =
| Parsed of 'b
| Failed of 'b * Errors.error
