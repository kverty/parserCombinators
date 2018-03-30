open Errors
open Stream
open Result

module K :
  sig
    type ('a, 'b, 'stream) t = 'a -> 'stream -> ('b, 'stream) result
    type ks

    val singleton : ('a, 'b, 'stream) t -> ks
    val add       : ('a, 'b, 'stream) t -> ks -> ks
    val fold      : (('a, 'b, 'stream) t -> ('b, 'stream) result -> ('b, 'stream) result) -> ks -> ('b, 'stream) result -> ('b, 'stream) result
    val empty     : ks
    val length    : ks -> int

  end =
  struct

    type ('a, 'b, 'stream) t = 'a -> 'stream -> ('b, 'stream) result

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

type ('a, 'b, 'stream) k       = ('a, 'b, 'stream) K.t
type ('a, 'b, 'stream) parser  = 'stream -> ('a, 'b, 'stream) k -> ('b, 'stream) result
type ('a, 'b, 'stream) parser' =            ('a, 'b, 'stream) k -> ('b, 'stream) result
