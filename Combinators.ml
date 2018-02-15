open Lazy
open Errors
open Types

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

let lift : ('a -> 'b) -> ('a, stream) result -> ('b, stream) result =
  fun f rs -> List.map (fun (a, s) -> (f a, s)) rs

let map : ('b -> 'c) -> (stream -> ('b, stream) result) -> (stream -> ('c, stream) result) =
  fun f p s -> List.map (fun (a, s) -> (f a, s)) (p s)

let opt : ('a, 'b) parser -> ('a, 'b) parser =
  fun p k s -> let s' = Oo.copy s in
               let k' = fun a s -> (k a s) @ (k a s') in (* нужно для 'a написать интерфейс *)
	       p k' s

let (<?>) = opt

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

(* filtering results *)
let lastErrorResult results =
try Some (List.fold_left (fun (r1, s1) (r2, s2) -> if Errors.pos (Errors.lastError s1 # errors) > Errors.pos (Errors.lastError s2 # errors) then (r1, s1) else (r2, s2))
                         (List.hd results)
		          results)
with _ -> None

let leastErrorResult results =
try Some (List.fold_left (fun (r1, s1) (r2, s2) -> if Errors.length s1 # errors < Errors.length s2 # errors then (r1, s1) else (r2, s2))
                         (List.hd results)
		          results)
with _ -> None

let correctResult results =
try List.fold_left (fun acc (r1, s1) -> if Errors.length s1 # errors = 0 then Some (r1, s1) else acc)
                   None
		   results
with _ -> None

(* printing results *)
let printLastErrorResult parser testNum testNumNum resType input =
  match lastErrorResult @@ parser (new stream @@ of_string @@ input) with
  | Some (res, s) -> Printf.printf ("Test %s.%s: %s parsed with result = " ^^ resType ^^ " and %s\n") testNum testNumNum input res (Errors.showError (Errors.lastError s # errors))
  | None          -> Printf.printf "Test %s.%s crushed\n" testNum testNumNum

let printLeastErrorResult parser testNum testNumNum resType input =
  match leastErrorResult @@ parser (new stream @@ of_string @@ input) with
  | Some (res, s) -> Printf.printf ("Test %s.%s: %s parsed with result = " ^^ resType ^^ " and %s\n") testNum testNumNum input res (Errors.show s # errors)
  | None          -> Printf.printf "Test %s.%s crushed\n" testNum testNumNum

let printAllResults parser testNum testNumNum resType input =
  List.iter (fun (res, s) -> Printf.printf ("Test %s.%s: %s parsed with result = " ^^ resType ^^ " and %s\n") testNum testNumNum input res (Errors.show s # errors)) (parser (new stream @@ of_string @@ input))

let printResult = printLeastErrorResult

let run : (stream -> ('b, stream) result) -> string -> 'b execResult =
  fun parser input ->
    let results = parser (new stream @@ of_string @@ input) in
    match correctResult results with
    | Some (res, s) -> Parsed res
    | None          -> (match lastErrorResult results with
                        | Some (res, s) -> Failed (res, (Errors.lastError s # errors)))
