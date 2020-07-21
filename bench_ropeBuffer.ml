open! Core
open Core_bench

module Buffer = Stdlib.Buffer

module type BufferLike = sig 
  type t
  
  val create : int -> t

  val contents : t -> string 

  val sub : t -> int -> int -> string
  val add_char  : t -> char -> unit 
  val add_string : t -> string -> unit
  val reset : t -> unit

end

module Make(M : BufferLike) = struct

  (* varying loop_times and str_len while keeping total_len
  constant gives some interesting difference between buffer
  ropeBuffer *)
  let loop_times = 100
  let str_len = 300
  let total_len = loop_times * str_len

  let build_str () = 
    let b1 = M.create 10 in
    for _ = 0 to loop_times do
      M.add_string b1 (String.make str_len 'a')
    done;
    b1
  let b1 = build_str ()

  let str_cat_clean () = 
    let b = M.create 10 in
    for _ = 0 to loop_times do
      M.add_string b (String.make str_len 'a')
    done;
    M.reset b


  let str_sub () =
    let sub_len = Random.int (str_len * 3) in
    let ofs = max 0 (Random.int (total_len - sub_len - 1)) in
    ignore(M.sub b1 ofs sub_len)
end

module BufferFunctions = Make(Buffer)
module RopeBufferFunctions = Make(RopeBuffer)
module CompactRopeBufferFunctions = Make(CompactRopeBuffer)

(* let () = 
  Command.run(Bench.make_command [  
    Bench.Test.create 
      ~name: "buffer_str_cat" BufferFunctions.str_cat_clean;
    Bench.Test.create 
      ~name: "ropeBuffer_str_cat" RopeBufferFunctions.str_cat_clean;
    Bench.Test.create 
      ~name: "compactRopeBuffer_str_cat" CompactRopeBufferFunctions.str_cat_clean;
  ]) *)

let () = 
  Command.run(Bench.make_command [  
    Bench.Test.create ~name: "buffer_str_sub" BufferFunctions.str_sub;
    Bench.Test.create ~name: "ropeBuffer_str_sub" RopeBufferFunctions.str_sub;
    Bench.Test.create ~name: "compactRopeBuffer_str_sub" CompactRopeBufferFunctions.str_sub;
  ])

