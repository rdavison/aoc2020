let usage () =
  let argv = Sys.get_argv () in
  Printf.failwithf "USAGE: %s <YEAR> <DAY> <PATH>" argv.(0) ()
;;

let () =
  let argv = Sys.get_argv () in
  let year, day, path =
    try
      let year = argv.(1) |> Int.of_string in
      let day = argv.(2) |> Int.of_string in
      let path = argv.(3) in
      year, day, path
    with
    | _ -> usage ()
  in
  match year with
  | 2020 ->
    (match day with
    | 1 -> Aoc2020.Cmd.day1 path
    | _ -> failwith "Unexpected day")
  | _ -> failwith "Unexpected year"
;;
