open Errors
open Stream
open Result

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
type ('a, 'b) parser  = stream -> ('a, 'b) k -> ('b, stream) result
type ('a, 'b) parser' =           ('a, 'b) k -> ('b, stream) result
