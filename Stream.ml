open Errors
open Result

let of_string s =
  let n = String.length s in
  let rec loop i =
    if i = n then [] else s.[i] :: loop (i + 1)
  in
  loop 0

let of_chars chars =
  let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf

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

    method equal : stream -> bool =
      fun s' -> (s = s' # chrs) && (p = s' # pos) && (Errors.equal errors (s' # errors))

    method look : 'b . string -> (string -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun cs k ->
        let rec loop chars result =
	  match chars with
	  | [] -> k (of_chars result)
	  | c :: tail -> fun s -> s # lookChar c (fun res s' -> loop tail (res :: result) s')
	in loop (of_string cs) [] self

    method lookChar : 'b . char -> (char -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun c k ->
      try
        if c = List.nth s p
        then k c {< p = p + 1 >}
        else begin
	  let err1 = Errors.Delete (List.nth s p, p) in
	  let err2 = Errors.Replace (c, p) in
	  let res1 = (match ({< p = p + 1; errors = Errors.addError err1 errors >} # lookChar c k) with
	              | Parsed (res, _) -> Failed (Some (res, err1))
		      | Failed x        -> Failed x) in
	  let res2 = (match (k c {< p = p + 1; errors =  Errors.addError err2 errors>}) with
	              | Parsed (res, _) -> Failed (Some (res, err2))
	              | Failed x        -> Failed x) in
	  res1 <@> res2 (*
          ({< p = p + 1; errors = Errors.addError (Errors.Delete (List.nth s p, p)) errors >} # look c k) <@>
	  (k c {< p = p + 1; errors =  Errors.addError (Errors.Replace (c, p)) errors>})*)
	end
      with _ -> emptyResult

    method getEOF : 'b . (unit -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        if p = List.length s
        then k () self
        else emptyResult
  end
