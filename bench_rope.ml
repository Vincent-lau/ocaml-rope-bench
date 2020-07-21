open! Core
open Core_bench

module type StringLike = sig
  type t
  val empty : t
  val iter : (char -> unit) -> t -> unit
  val map : (char -> char) -> t -> t
  val make : int -> char -> t
  val (^) : t -> t -> t
end

module StringExtended = struct
  include String

  type t = string
  let (^) = (^)

  let iter f s = String.iter ~f:f s

  let map f s = String.map ~f:f s
  let empty = ""
end


module Make (M : StringLike) = struct

  let list_len = 1000
  let len = 1000000

  (* helper functions *)
  let f_it = (fun (c:char) -> ignore(c))
  let f_map = (fun (c:char) -> c)

  

  let rec build_list len acc = 
    if (len = 0) then 
      acc
    else
      let sl = Random.int 50 in 
      let rs = M.make sl 'b' in
      build_list (len - 1) (rs :: acc)

  let build_rope len alt=
    let rec build_alternate len acc lr_flag =
      if(len = 0) then
        acc
      else
        let r_l = min (Random.int 20) len in
        let r = M.make r_l 'c' in
        if lr_flag then 
          build_alternate (len-r_l) (M.(^) acc r) (not lr_flag)
        else
          build_alternate (len-r_l) (M.(^) r acc) (not lr_flag)
    in
    let rec build_unbalanced len acc =
      if(len = 0) then
        acc
      else
        let r_l = min (Random.int 20) len in
        let r = M.make r_l 'c' in
          build_unbalanced (len-r_l) (M.(^) r acc) 
    in
    if alt then build_alternate len M.empty true
    else build_unbalanced len M.empty

  (* test data *)
  let cat_list = build_list list_len []
  let plain_str = M.make len 'c'
  let alt_rope = build_rope len true
  let unbalanced_rope = build_rope len false


  (* test functions *)
  let cat = fun () -> ignore (List.fold ~init:M.empty ~f:M.(^) cat_list)

  let iter_str = fun () -> ignore(M.iter f_it plain_str)
  let map_str = fun ()-> ignore(M.iter f_it plain_str)
  let iter_alt = fun () -> ignore(M.iter f_it alt_rope)
  let map_alt = fun () -> ignore(M.map f_map alt_rope)
  let iter_unbalanced = fun () -> ignore(M.iter f_it unbalanced_rope)
  let map_unbalanced = fun () -> ignore(M.map f_map unbalanced_rope)

end



module StringFunctions = Make(StringExtended)
module RopeFunctions = Make(Rope)
module CRopeFunctions = Make(CompactRope)

let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"string_cat" StringFunctions.cat;
    Bench.Test.create ~name:"rope_cat" RopeFunctions.cat;
    Bench.Test.create ~name:"crope_cat" CRopeFunctions.cat; 
  ])
;;
let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"string_iter" StringFunctions.iter_str;
    Bench.Test.create ~name:"rope_iter_alt" RopeFunctions.iter_alt;
    Bench.Test.create ~name:"crope_iter_alt" CRopeFunctions.iter_alt; 
  ])
;;
let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"string_map" StringFunctions.map_str;
    Bench.Test.create ~name:"rope_map_alt" RopeFunctions.map_alt;
    Bench.Test.create ~name:"crope_map_alt" CRopeFunctions.map_alt; 
  ])
;;
let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"string_iter" StringFunctions.map_str;
    Bench.Test.create ~name:"rope_iter_unbalanced" RopeFunctions.map_unbalanced;
    Bench.Test.create ~name:"crope_iter_unbalanced" CRopeFunctions.map_unbalanced; 
  ])
