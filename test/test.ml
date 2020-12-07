let%expect_test "day 1" =
  let input = Aoc2020.Day1.of_string [%blob "day/1/input"] in
  let to_string x =
    match x with
    | None -> "Not found"
    | Some num -> Printf.sprintf "%d" num
  in
  printf "%s" (Aoc2020.Day1.part1 input |> to_string);
  [%expect {| 357504 |}];
  printf "%s" (Aoc2020.Day1.part2 input |> to_string);
  [%expect {| 12747392 |}]
;;
