type t = int list

let of_string x =
  x
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
         match String.strip line with
         | "" -> None
         | line -> Some (Int.of_string line))
;;

let rec run l =
  let pred x y = Int.equal 2020 (x + y) in
  match l with
  | [] -> failwith "Not found"
  | x :: rest ->
    (match List.find rest ~f:(pred x) with
    | Some y -> x * y
    | None -> run rest)
;;

let%expect_test "given example" =
  printf "%d" (run [ 1721; 979; 366; 299; 675; 1456 ]);
  [%expect {| 514579 |}]
;;
