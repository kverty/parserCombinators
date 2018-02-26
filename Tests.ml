open Combinators
open Errors
open Stream
open Types
open Result

let printStringResult parser testNum testNumNum input =
  match parser (new stream @@ of_string @@ input) with
  | Parsed ((res, s), opt)          -> Printf.printf ("Test %s.%s: %s parsed with result = %s\n") testNum testNumNum input res
  | Failed (Some ((res, s), error)) -> Printf.printf ("Test %s.%s: %s parsed with result = %s and has an error : %s\n") testNum testNumNum input res (Errors.showError error)
  | Failed None                     -> Printf.printf "Test %s.%s crushed\n" testNum testNumNum

let printCharResult parser testNum testNumNum input =
  match parser (new stream @@ of_string @@ input) with
  | Parsed ((res, s), opt)          -> Printf.printf ("Test %s.%s: %s parsed with result = %c\n") testNum testNumNum input res
  | Failed (Some ((res, s), error)) -> Printf.printf ("Test %s.%s: %s parsed with result = %c and has an error : %s\n") testNum testNumNum input res (Errors.showError error)
  | Failed None                     -> Printf.printf "Test %s.%s crushed\n" testNum testNumNum

let printIntResult parser testNum testNumNum input =
  match parser (new stream @@ of_string @@ input) with
  | Parsed ((res, s), opt)          -> Printf.printf ("Test %s.%s: %s parsed with result = %d\n") testNum testNumNum input res
  | Failed (Some ((res, s), error)) -> Printf.printf ("Test %s.%s: %s parsed with result = %d and has an error : %s\n") testNum testNumNum input res (Errors.showError error)
  | Failed None                     -> Printf.printf "Test %s.%s crushed\n" testNum testNumNum

let _ =
  let test1 s = ((fun stream' -> stream' # look 'a') |> (fun xa ->
	         (fun stream' -> stream' # look 'b') |> (fun xb ->
		  return (xa :: xb :: [])))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test1 "01" "1" "ab" in
           printStringResult test1 "01" "2" "ac"

let _ =
  let test2 s = ((fun stream' -> (stream' # look 'a')) <|> (fun stream' -> (stream' # look 'b'))) s (fun res s -> Parsed ((res, s), None)) in
    let () = printCharResult test2 "02" "1" "b" in
             printCharResult test2 "02" "2" "c"

let _ =
  let pretest3 = (fix (fun p -> let neWord = (fun stream' -> (stream' # look 'a')) |> (fun xa ->
					     p                                     |> (fun xp ->
					     return (xa :: xp))) in
                                    neWord <|> ((fun stream' -> (stream' # look 'a')) |> fun xa ->
				                 return (xa :: [])))) in
  let test3 s = (pretest3 |> (fun res -> (fun stream' -> (stream' # getEOF)) |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test3 "03" "1" "aaa" in
	   printStringResult test3 "03" "2" "aaaba"

let terminal : char -> (char, 'b) parser =
  fun c stream' k -> (stream' # look c) k

let eof : (unit, 'b) parser =
  fun stream' k -> (stream' # getEOF) k

let _ =
  let pretest4 = (fix (fun p -> let neWord = p            |> (fun xp ->
				             terminal 'a' |> (fun xa ->
				             return (xp @ (xa :: [])))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                return (xa :: [])))) in
  let test4 s = (pretest4 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test4 "04" "1" "aaa" in
           printStringResult test4 "04" "2" "aaaba"

let _ =
  let pretest5 = (fix (fun p -> let neWord = p            |> (fun xp1    ->
				             terminal '+' |> (fun xplus  ->
					     p            |> (fun xp2    ->
				             return (xp1 @ (xplus :: xp2))))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                return (xa :: []))))  in
  let test5 s = (pretest5 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test5 "05" "1" "a+a+a+a" in
	   printStringResult test5 "05" "2" "a+a+a+b+a"

let _ =
  let pretest6 = (fix (fun p -> let aWord = terminal 'a' |> (fun _  ->
				            p            |> (fun xp ->
					    terminal 'a' |> (fun _  ->
				            return (1 + xp)))) and
				     bWord = terminal 'b' |> (fun _  ->
				   	     p            |> (fun xp ->
				   	     terminal 'b' |> (fun _  ->
				   	     return (1 + xp)))) and
				     cWord = terminal 'c' |> (fun _  ->
				             p            |> (fun xp ->
				      	     terminal 'c' |> (fun _  ->
				      	     return (1 + xp)))) in
				     aWord <|> bWord <|> cWord     <|>
				     (empty |> fun _ -> return 0) <|>
				     ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun _ -> return 1))) in
  let test6 s = (pretest6 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((res, s), None)) in
  let () = printIntResult test6 "06" "1" "abba" in
  let () = printIntResult test6 "06" "2" "abcba" in
	   printIntResult test6 "06" "3" "abac"

let _ =
  let rec pretest7 k = (fix (fun p -> let plusWord = p            |> (fun xp    ->
				                     terminal '+' |> (fun xplus ->
				      	             mulli        |> (fun xm    ->
				      	             return (xp @ (xplus :: xm))))) in
				          mulli <|> plusWord)) @@ k and
	  mulli    k = (fix (fun p -> let multWord = p            |> (fun xp    ->
					             terminal '*' |> (fun xmult ->
					      	     primary      |> (fun xpr   ->
					      	     return (xp @ (xmult :: xpr))))) in
					primary <|> multWord)) @@ k and
	  primary  k = ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun xt -> return (xt :: [])) @@ k in
  let test7 s = (pretest7 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test7 "07" "1" "a+b+c" in
  let () = printStringResult test7 "07" "2" "a*b+c" in
  let () = printStringResult test7 "07" "3" "a+b+*c" in
	   printStringResult test7 "07" "4" "a+b+"

let _ =
  let expr n s =
    let rec e k = (fix (fun s -> let plusWord = s              |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        return (xs @ (xplus :: xm))))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	return (xs @ (xmult :: xpr))))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun xbr1 ->
	  		  e            |> (fun xe   ->
	  		  terminal ')' |> (fun xbr2 ->
	  		  return ((xbr1 :: xe) @ (xbr2 :: []))))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult (expr (terminal 'n' |> fun xt -> return (xt :: []))) "08" "1" "(n-n)" in
  let () = printStringResult (expr (terminal 'n' |> fun xt -> return (xt :: []))) "08" "2" "(n--n)" in
  let () = printStringResult (expr ((terminal 'a' <|> terminal 'b') |> fun xt -> return (xt :: []))) "08" "3" "(b+a)" in
	   printStringResult (expr ((terminal 'a' <|> terminal 'b') |> fun xt -> return (xt :: []))) "08" "4" "a+"

let _ =
  let rec pretest9 k = (fix (fun p -> (p |> (fun xp1 ->
	                               p |> (fun xp2 ->
				       p |> (fun xp3 ->
				       return (xp1 @ xp2 @ xp3))))) <|>
				      (p |> (fun xp1 ->
				       p |> (fun xp2 ->
				       return (xp1 @ xp2)))) <|>
				      (terminal 'b' |> fun xb -> return (xb :: [])) <|>
				      (empty |> fun _ -> return []))) @@ k in
  let test9 s = (pretest9 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test9 "09" "1" "bbbbbbbbbbb" in
	   printStringResult test9 "09" "2" "bbbcb"

let _ =
  let pretest10 = manyFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test10 s = (pretest10 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test10 "10" "1" "bbbbbbbbb" in
  let () = printStringResult test10 "10" "2" "bbbcb" in
	   printStringResult test10 "10" "3" ""

let _ =
  let pretest11 = someFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test11 s = (pretest11 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test11 "11" "1" "bbbbbbbbb" in
           printStringResult test11 "11" "2" "bbbcb"

let _ =
  let expr n s =
    let rec e k = (fix (fun s -> let plusWord = s            |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        return (if xplus = '+' then xs + xm else xs - xm)))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	return (if xmult = '*' then xs * xpr else xs / xpr)))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun _  ->
	  		  e            |> (fun xe ->
	  		  terminal ')' |> (fun _  ->
	  		  return xe))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((res, s), None)) in
  printIntResult (expr ((terminal '0' <|> terminal '1' <|> terminal '2' <|> terminal '3') |> (fun t -> return ((Char.code t) - 48)))) "12" "1" "2*3+1"

let _ =
  let expr = (fix (fun p -> let neWord = p            |> (fun xp1    ->
				         terminal '+' |> (fun xplus  ->
					 p            |> (fun xp2    ->
				         return (xp1 @ (xplus :: xp2))))) in
				neWord <|> (terminal 'a' |> fun xa ->
				            return (xa :: [])))) in
  let pretest13 = (fix (fun p -> let neWord = expr         |> (fun xe     ->
				              terminal ' ' |> (fun xspace ->
					      p            |> (fun xp     ->
				              return (xe @ (xspace :: xp))))) in
      			             neWord <|> (expr |> fun xe -> return xe))) in
  let test13 s = (pretest13 |> (fun res -> eof |> (fun _ -> return res))) s (fun res s -> Parsed ((of_chars res, s), None)) in
  let () = printStringResult test13 "13" "1" "a+a+a+a" in
	   printStringResult test13 "13" "2" "a+b a+a a*a"
