open! Core
open Core_bench

let (^^) = Rope.(^)

let (^^^) = CompactRope.(^)


let build_string len = 
  let b = Bytes.create len in
  for i = 0 to len - 1 do 
    Bytes.set b i (char_of_int (Random.int 255))
  done;
  Bytes.to_string b

let rec build_list len acc = 
  if (len = 0) then 
    acc
  else
    let sl = Random.int 50 in 
    let rs = build_string sl in
    build_list (len - 1) (rs :: acc)


(* this way of building is actually not perfectly balanced
all I am doing is alternating which side it is concatnated *)

let rec build_rope len acc lr_flag=
  if(len = 0) then
    acc
  else
    let r_l = min (Random.int 20) len in
    let r = Rope.of_string (build_string r_l) in
    if lr_flag then 
      build_rope (len-r_l) (acc ^^ r) (not lr_flag)
    else
      build_rope (len-r_l) (r ^^ acc) (not lr_flag)


let rec build_rope_unbalanced len acc =
  if(len = 0) then
    acc
  else
    let r_l = min (Random.int 20) len in
    let r = Rope.of_string (build_string r_l) in
      build_rope_unbalanced (len-r_l) (r ^^ acc) 
      

let rec build_compactRope len acc lr_flag=
  if(len = 0) then
    acc
  else
    let r_l = min (Random.int 20) len in
    let rope = CompactRope.of_string (build_string r_l) in
    if lr_flag then 
      build_compactRope (len-r_l) (acc ^^^ rope) (not lr_flag)
    else
      build_compactRope (len-r_l) (rope ^^^ acc) (not lr_flag)

let test_rope = build_rope 1000000 Rope.empty true
let test_crope = build_compactRope 1000000 CompactRope.empty true
let test_str = build_string 1000000 


(* test for concat *)
let () =
  let test_str_list = build_list 1000 [] in
  let test_rope_list = List.map test_str_list ~f:(Rope.of_string) in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"String concat" (fun () -> 
      ignore (List.fold test_str_list ~init:"" ~f:(^) ));
    Bench.Test.create ~name:"Rope concat" (fun () ->
      ignore (List.fold test_rope_list ~init:Rope.empty ~f:Rope.(^) ));
  ])


(* test iter string vs compactRope  *)
let () =
  let f_it = (fun (c:char) -> ignore(c))
  and f_map = (fun (c:char) -> c) in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"String iter" (fun () -> 
      ignore (String.iter ~f:f_it test_str));
    Bench.Test.create ~name:"Rope iter" (fun () ->
      ignore (Rope.iter f_it test_rope));
    Bench.Test.create ~name:"CompactRope iter" (fun () ->
      ignore (CompactRope.iter f_it test_crope));
    Bench.Test.create ~name:"String map" (fun () -> 
      ignore (String.map ~f:f_map test_str));
    Bench.Test.create ~name:"Rope map" (fun () ->
      ignore (Rope.map f_map test_rope));
    Bench.Test.create ~name:"CompactRope map" (fun () -> 
      ignore (CompactRope.map f_map test_crope));
  ])



