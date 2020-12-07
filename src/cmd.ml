let rec day1 l =
  let pred x y = Int.equal 2020 (x + y) in
  match l with
  | [] -> failwith "Not found"
  | x :: rest ->
    (match List.find rest ~f:(pred x) with
    | Some y -> x * y
    | None -> day1 rest)
;;

let%expect_test "day1 example" = printf "%d" (day1 [ 1721; 979; 366; 299; 675; 1456 ]);
  [%expect {| 514579 |}]
