let%expect_test "day 1" =
  let input = Aoc.Day1.of_string [%blob "day/1/input"] in
  let to_string x =
    match x with
    | None -> "Not found"
    | Some num -> Printf.sprintf "%d" num
  in
  printf "%s" (Aoc.Day1.part1 input |> to_string);
  [%expect {| 357504 |}];
  printf "%s" (Aoc.Day1.part2 input |> to_string);
  [%expect {| 12747392 |}]
;;

let%expect_test "day 2" =
  let input = Aoc.Day2.of_string [%blob "day/2/input"] in
  let to_string x = Int.to_string x in
  printf "%s" (Aoc.Day2.part1 input |> to_string);
  [%expect {| 572 |}]
;;
