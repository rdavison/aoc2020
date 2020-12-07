type t = int list

let of_string x =
  x
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
         match String.strip line with
         | "" -> None
         | line -> Some (Int.of_string line))
;;

let adds_to_2020 x y = Int.equal 2020 (x + y)

let rec part1 l =
  match l with
  | [] -> failwith "Not found"
  | x :: rest ->
    (match List.find rest ~f:(adds_to_2020 x) with
    | Some y -> x * y
    | None -> part1 rest)
;;

let%expect_test "part1 example" =
  printf "%d" (part1 [ 1721; 979; 366; 299; 675; 1456 ]);
  [%expect {| 514579 |}]
;;
