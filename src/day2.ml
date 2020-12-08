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

let run t ~is_valid =
  List.fold t ~init:0 ~f:(fun i entry -> if is_valid entry then i + 1 else i)
;;

let part1 t =
  run t ~is_valid:(fun entry ->
      let count =
        String.fold entry.password ~init:0 ~f:(fun count c ->
            if Char.equal c entry.policy.letter then count + 1 else count)
      in
      entry.policy.low <= count && count <= entry.policy.high)
;;

let part2 t =
  run t ~is_valid:(fun entry ->
      String.fold_until
        entry.password
        ~init:(1, `Init)
        ~finish:(fun (_i, state) ->
          match state with
          | `Init -> false
          | `Ok -> true)
        ~f:(fun (i, state) c ->
          let continue state = Continue_or_stop.Continue (i + 1, state) in
          let pass_thru = continue state in
          let fail = Continue_or_stop.Stop false in
          let can_transition_state =
            let in_bounds = i = entry.policy.low || i = entry.policy.high in
            let letter_match = Char.equal c entry.policy.letter in
            in_bounds && letter_match
          in
          match can_transition_state with
          | false -> pass_thru
          | true ->
            (match state with
            | `Init -> continue `Ok
            | `Ok -> fail)))
;;

let%expect_test "given examples" =
  let data =
    [ { policy = { low = 1; high = 3; letter = 'a' }; password = "abcde" }
    ; { policy = { low = 1; high = 3; letter = 'b' }; password = "cdefg" }
    ; { policy = { low = 2; high = 9; letter = 'c' }; password = "ccccccccc" }
    ]
  in
  printf "%d\n" (part1 data);
  [%expect {| 2 |}];
  printf "%d\n" (part2 data);
  [%expect {| 1 |}]
;;
