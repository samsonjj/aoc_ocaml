let file = "input.txt"

let ic: in_channel = open_in file
let lines: string list = In_channel.fold_lines
  (fun (acc: string list) (line: string) -> line::acc)
  []
  ic
let nums: int list = List.map int_of_string lines

(* Part 1 *)
let () = 
  let a: int = List.find (fun x -> List.mem (2020 - x) nums) nums in
  Printf.printf "%d\n" ((2020 - a) * a)

(* Part 2 *)
let () =
  let result: int option = List.find_map
    (fun x ->
      List.find_map
        (fun y -> 
          List.find_map
            (fun z -> if x + y + z = 2020 then Some (x * y * z) else None)
            nums
        )
        nums
    )
    nums in

  match result with
    | Some x -> Printf.printf "%d\n" x
    | _ -> raise Not_found

