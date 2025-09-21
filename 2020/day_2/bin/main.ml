type policy = (int * int * char * string)

let ic: in_channel = open_in "input.txt"
let lines: string list = In_channel.fold_lines (fun acc line -> line::acc) [] ic

(*
  Example policy:
  3-15 x: htblqcbdxdzgvhszxz
*)
let range_of_string s =
  let parts = String.split_on_char '-' s in
  match parts with
    | low::high::_ -> (int_of_string low, int_of_string high)
    | _ -> raise (Invalid_argument "invalid range")

let policy_of_string (s: string) : policy =
  let parts = String.split_on_char ' ' s in
  match parts with
    | range::ccolon::pass::_ ->
      let (low, high) = range_of_string range in
      let c = ccolon.[0] in
      (low, high, c, pass)
    | _ -> raise Not_found

let countc (c: char) (s: string) = 
  String.fold_left (fun count c' -> if c = c' then count + 1 else count) 0 s



let policies = List.map policy_of_string lines

(* Part 1 *)
let is_valid (p: policy): bool = 
  let low, high, c, pass = p in
  let count = countc c pass in
  count >= low && count <= high

let () =
  let answer = List.filter is_valid policies
    |> List.length in
  
  Printf.printf "%d\n" answer

(* Part 2 *)
let is_valid_2 (p: policy): bool = 
  let low, high, c, pass = p in
  let a = pass.[low-1] = c in
  let b = pass.[high-1] = c in
  a <> b

let () =
  let answer = List.filter is_valid_2 policies
    |> List.length in
  
  Printf.printf "%d\n" answer

 