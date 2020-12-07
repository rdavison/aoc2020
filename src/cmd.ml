let day1 ~dir =
  let input =
    Stdlib.Filename.concat dir "input" |> In_channel.read_all |> Day1.of_string
  in
  let part1 = Day1.part1 input in
  (* let part2 = Day1.part2 input in *)
  printf "Part One: %d\n" part1;
  (* printf "Part Two: %d\n" part2 *)
  ()
;;
