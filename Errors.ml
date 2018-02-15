module Errors:
  sig
    type error =
    | Delete of char * int
    | Replace of char * int

    type t = error list

    val showError : error -> string
    val pos       : error -> int
    val lastError : t -> error
    val addError  : error -> t -> t
    val empty     : t
    val length    : t -> int
    val show      : t -> string
    val equal     : t -> t -> bool

  end =
  struct
    type error =
    | Delete of char * int
    | Replace of char * int

    let showError err = match err with
    			| Delete (c, pos)  -> Printf.sprintf "#char %c was deleted on pos %d#\n" c (pos + 1)
    			| Replace (c, pos) -> Printf.sprintf "#char %c was replaced with on pos %d#\n" c (pos + 1)

    let equalErr err1 err2 = match err1, err2 with
                             | Delete (c1, pos1),  Delete (c2, pos2)  when c1 = c2 && pos1 = pos2 -> true
			     | Replace (c1, pos1), Replace (c2, pos2) when c1 = c2 && pos1 = pos2 -> true
			     | _, _ -> false

    let pos err = match err with
    		  | Delete (_, pos)  -> (pos + 1)
    		  | Replace (_, pos) -> (pos + 1)

    type t = error list

    let lastError    errs = List.hd errs
    let addError err errs = err :: errs
    let empty             = []
    let length            = List.length
    let show         errs = List.fold_left (fun acc err -> acc ^ showError err) (Printf.sprintf "%d errors:\n" (List.length errs)) errs
    let equal errs1 errs2 = try List.fold_right2 (fun err1 err2 acc -> acc && (equalErr err1 err2)) errs1 errs2 true
                            with _ -> false
  end
