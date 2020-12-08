type policy =
  { low : int
  ; high : int
  ; letter : char
  }
[@@deriving sexp]

type entry =
  { policy : policy
  ; password : string
  }
[@@deriving sexp]

type t = entry list [@@deriving sexp]

let parse_policy x =
  let lowhigh, letter = String.lsplit2_exn x ~on:' ' in
  let low, high = String.lsplit2_exn lowhigh ~on:'-' in
  { low = Int.of_string low; high = Int.of_string high; letter = letter.[0] }
;;

let parse_entry line =
  let policy, password = String.lsplit2_exn line ~on:':' in
  let password = String.strip password in
  let policy = parse_policy policy in
  { policy; password }
;;

let of_string x =
  x
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
         match String.strip line with
         | "" -> None
         | line -> Some (parse_entry line))
;;

let%expect_test "of_string example" =
  let data =
    String.concat ~sep:"\n" [ "1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc" ]
  in
  let t = of_string data in
  print_s (sexp_of_t t);
  [%expect
    {|
    (((policy ((low 1) (high 3) (letter a))) (password abcde))
     ((policy ((low 1) (high 3) (letter b))) (password cdefg))
     ((policy ((low 2) (high 9) (letter c))) (password ccccccccc))) |}]
;;

let is_valid (entry : entry) =
  let count =
    String.fold entry.password ~init:0 ~f:(fun i c ->
        if Char.equal c entry.policy.letter then i + 1 else i)
  in
  entry.policy.low <= count && count <= entry.policy.high
;;

let run t = List.fold t ~init:0 ~f:(fun i entry -> if is_valid entry then i + 1 else i)

let%expect_test "given examples" =
  let data =
    [ { policy = { low = 1; high = 3; letter = 'a' }; password = "abcde" }
    ; { policy = { low = 1; high = 3; letter = 'b' }; password = "cdefg" }
    ; { policy = { low = 2; high = 9; letter = 'c' }; password = "ccccccccc" }
    ]
  in
  printf "%d\n" (run data);
  [%expect {| 2 |}]
;;
