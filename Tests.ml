open Combinators
open Types
open Errors

let _ =
  let test1 = ((fun k stream' -> stream' # look 'a' k) |> (fun xa ->
	       (fun k stream' -> stream' # look 'b' k) |> (fun xb ->
		success (xa :: xb :: [])))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test1 "01" "1" "%s" "ab" in
           printResult test1 "01" "2" "%s" "ac"

let _ =
  let test2 = ((fun k stream' -> (stream' # look 'a') k) <|> (fun k stream' -> (stream' # look 'b') k)) (fun res s -> [(res, s)]) in
    let () = printResult test2 "02" "1" "%c" "b" in
             printResult test2 "02" "2" "%c" "c"

let _ =
  let pretest3 = (fix (fun p -> let neWord = (fun k stream' -> (stream' # look 'a') k) |> (fun xa ->
					     p                                         |> (fun xp ->
					     success (xa :: xp))) in
                                    neWord <|> ((fun k stream' -> (stream' # look 'a') k) |> fun xa ->
				                 success (xa :: [])))) in
  let test3 = (pretest3 |> (fun res -> (fun k stream' -> (stream' # getEOF) k) |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test3 "03" "1" "%s" "aaa" in
	   printResult test3 "03" "2" "%s" "aaaba"

let terminal : char -> (char, 'b) parser =
  fun c k stream' -> (stream' # look c) k

let eof : (unit, 'b) parser =
  fun k stream' -> (stream' # getEOF) k

let _ =
  let pretest4 = (fix (fun p -> let neWord = p            |> (fun xp ->
				             terminal 'a' |> (fun xa ->
				             success (xp @ (xa :: [])))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                success (xa :: [])))) in
  let test4 = (pretest4 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test4 "04" "1" "%s" "aaa" in
           printResult test4 "04" "2" "%s" "aaaba"

let _ =
  let pretest5 = (fix (fun p -> let neWord = p            |> (fun xp1    ->
				             terminal '+' |> (fun xplus  ->
					     p            |> (fun xp2    ->
				             success (xp1 @ (xplus :: xp2))))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                success (xa :: []))))  in
  let test5 = (pretest5 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test5 "05" "1" "%s" "a+a+a+a" in
	   printResult test5 "05" "2" "%s" "a+a+a+b+a"

let _ =
  let pretest6 = (fix (fun p -> let aWord = terminal 'a' |> (fun _  ->
				            p            |> (fun xp ->
					    terminal 'a' |> (fun _  ->
				            success (1 + xp)))) and
				     bWord = terminal 'b' |> (fun _  ->
				   	     p            |> (fun xp ->
				   	     terminal 'b' |> (fun _  ->
				   	     success (1 + xp)))) and
				     cWord = terminal 'c' |> (fun _  ->
				             p            |> (fun xp ->
				      	     terminal 'c' |> (fun _  ->
				      	     success (1 + xp)))) in
				     aWord <|> bWord <|> cWord     <|>
				     (empty |> fun _ -> success 0) <|>
				     ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun _ -> success 1))) in
  let test6 = (pretest6 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  let () = printResult test6 "06" "1" "%d" "abba" in
  let () = printResult test6 "06" "2" "%d" "abcba" in
	   printResult test6 "06" "3" "%d" "abac"

let _ =
  let rec pretest7 k = (fix (fun p -> let plusWord = p            |> (fun xp    ->
				                     terminal '+' |> (fun xplus ->
				      	             mulli        |> (fun xm    ->
				      	             success (xp @ (xplus :: xm))))) in
				          mulli <|> plusWord)) @@ k and
	  mulli    k = (fix (fun p -> let multWord = p            |> (fun xp    ->
					             terminal '*' |> (fun xmult ->
					      	     primary      |> (fun xpr   ->
					      	     success (xp @ (xmult :: xpr))))) in
					primary <|> multWord)) @@ k and
	  primary  k = ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun xt -> success (xt :: [])) @@ k in
  let test7 = (pretest7 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test7 "07" "1" "%s" "a+b+c" in
  let () = printResult test7 "07" "2" "%s" "a*b+c" in
  let () = printResult test7 "07" "3" "%s" "a+b+*c" in
	   printResult test7 "07" "4" "%s" "a+b+"

let _ =
  let expr n =
    let rec e k = (fix (fun s -> let plusWord = s              |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        success (xs @ (xplus :: xm))))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	success (xs @ (xmult :: xpr))))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun xbr1 ->
	  		  e            |> (fun xe   ->
	  		  terminal ')' |> (fun xbr2 ->
	  		  success ((xbr1 :: xe) @ (xbr2 :: []))))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult (expr (terminal 'n' |> fun xt -> success (xt :: []))) "08" "1" "%s" "(n-n)" in
  let () = printResult (expr (terminal 'n' |> fun xt -> success (xt :: []))) "08" "2" "%s" "(n--n)" in
  let () = printResult (expr ((terminal 'a' <|> terminal 'b') |> fun xt -> success (xt :: []))) "08" "3" "%s" "(b+a)" in
	   printResult (expr ((terminal 'a' <|> terminal 'b') |> fun xt -> success (xt :: []))) "08" "4" "%s" "a+"

let _ =
  let rec pretest9 k = (fix (fun p -> (p |> (fun xp1 ->
	                               p |> (fun xp2 ->
				       p |> (fun xp3 ->
				       success (xp1 @ xp2 @ xp3))))) <|>
				      (p |> (fun xp1 ->
				       p |> (fun xp2 ->
				       success (xp1 @ xp2)))) <|>
				      (terminal 'b' |> fun xb -> success (xb :: [])) <|>
				      (empty |> fun _ -> success []))) @@ k in
  let test9 = (pretest9 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test9 "09" "1" "%s" "bbbbbbbbbbb" in
	   printResult test9 "09" "2" "%s" "bbbcb"

let _ =
  let pretest10 = manyFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test10 = (pretest10 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test10 "10" "1" "%s" "bbbbbbbbb" in
  let () = printResult test10 "10" "2" "%s" "bbbcb" in
	   printResult test10 "10" "3" "%s" ""

let _ =
  let pretest11 = someFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test11 = (pretest11 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test11 "11" "1" "%s" "bbbbbbbbb" in
           printResult test11 "11" "2" "%s" "bbbcb"

let _ =
  let expr n =
    let rec e k = (fix (fun s -> let plusWord = s            |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        success (if xplus = '+' then xs + xm else xs - xm)))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	success (if xmult = '*' then xs * xpr else xs / xpr)))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun _  ->
	  		  e            |> (fun xe ->
	  		  terminal ')' |> (fun _  ->
	  		  success xe))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  printResult (expr ((terminal '0' <|> terminal '1' <|> terminal '2' <|> terminal '3') |> (fun t -> success ((Char.code t) - 48)))) "12" "1" "%d" "2*3"

let _ =
  let expr = (fix (fun p -> let neWord = p            |> (fun xp1    ->
				         terminal '+' |> (fun xplus  ->
					 p            |> (fun xp2    ->
				         success (xp1 @ (xplus :: xp2))))) in
				neWord <|> (terminal 'a' |> fun xa ->
				            success (xa :: [])))) in
  let pretest13 = (fix (fun p -> let neWord = expr         |> (fun xe     ->
				              terminal ' ' |> (fun xspace ->
					      p            |> (fun xp     ->
				              success (xe @ (xspace :: xp))))) in
      			             neWord <|> (expr |> fun xe -> success xe))) in
  let test13 = (pretest13 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(of_chars res, s)]) in
  let () = printResult test13 "13" "1" "%s" "a+a+a+a" in
	   printResult test13 "13" "2" "%s" "a+b a+a a*a"
