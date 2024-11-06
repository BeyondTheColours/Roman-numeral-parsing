type ('a, 'e) result =
  |Ok of 'a
  |Err of 'e
;;

let rns_to_nums rn =
  match rn with
  |'I' -> Ok(1)
  |'V' -> Ok(5)
  |'X' -> Ok(10)
  |'L' -> Ok(50)
  |'C' -> Ok(100)
  |_ -> Err("Unrecognised character : "^Char.escaped rn)
;;

let explode_rn s =
  let rec aux s count res =
    if count = 0 then (rns_to_nums s.[count])::res
    else aux s (count-1) ((rns_to_nums (s.[count]))::res)
  in aux s ((String.length(s)) - 1) []
;;

let rec eval_num_list l =
  match l with
  |[] -> Err("No numerals given!")
  |h::[] -> h
  |h::t ->
      match h with
      |Err(e) -> Err(e)
      |Ok(v) ->
          match t with
          |[] -> Err("No numerals given!")
          |h1::[] ->
              begin
                match h1 with
                |Err(e) -> Err(e)
                |Ok(v1) -> if v < v1 then Ok(v1-v) else Ok(v1+v)
                
              end
          |h1::t1 ->
              match h1 with
              |Err(e) -> Err(e)
              |Ok(v1) ->
                  begin
                    if v < v1 then
                      match eval_num_list t1 with
                      |Err(e) -> Ok(v1-v)
                      |Ok(v2) -> Ok(v+v1+v2) 
                    else
                      match eval_num_list t with
                      (*Returning this Err(e) is the one which ensures an unrecognised character gets returned*)
                      |Err(e) -> Err(e)
                      |Ok(v2) -> Ok(v+v2)
                  end
              
;;

let rn_to_number rn = rn |> explode_rn |> eval_num_list;;
