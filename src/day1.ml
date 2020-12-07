type t = int list

let of_string x =
  x
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
         match String.strip line with
         | "" -> None
         | line -> Some (Int.of_string line))
;;

let sums_to_2020 l = Int.equal 2020 (List.fold l ~init:0 ~f:( + ))

let part1 l =
  let rec search l =
    match l with
    | [] -> None
    | x :: ys ->
      (match List.find ys ~f:(fun y -> sums_to_2020 [ x; y ]) with
      | Some y -> Some (x * y)
      | None -> search ys)
  in
  search l
;;

let part2 l =
  let rec search l =
    match l with
    | [] | [ _ ] -> None
    | x :: y :: zs ->
      (match List.find zs ~f:(fun z -> sums_to_2020 [ x; y; z ]) with
      | Some z -> Some (x * y * z)
      | None -> List.find_map [ x :: zs; y :: zs; zs ] ~f:search)
  in
  List.sort l ~compare:Int.compare |> search
;;

let%expect_test "given examples" =
  let data = [ 1721; 979; 366; 299; 675; 1456 ] in
  let runtest f =
    match f data with
    | None -> printf "Not Found"
    | Some num -> printf "%d\n" num
  in
  runtest part1;
  [%expect {| 514579 |}];
  runtest part2;
  [%expect {| 241861950 |}]
;;
