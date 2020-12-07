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

let rec part1 l =
  match l with
  | [] -> failwith "Not found"
  | x :: rest ->
    (match List.find rest ~f:(fun y -> sums_to_2020 [ x; y ]) with
    | Some y -> x * y
    | None -> part1 rest)
;;

let part2 l =
  let rec loop l =
    match l with
    | [] -> None
    | x :: ys ->
      (match ys with
      | [] -> None
      | y :: zs ->
        (match List.find zs ~f:(fun z -> sums_to_2020 [ x; y; z ]) with
        | Some z -> Some (x * y * z)
        | None ->
          (match loop (x :: zs) with
          | Some a -> Some a
          | None ->
            (match loop (y :: zs) with
            | Some a -> Some a
            | None -> loop zs))))
  in
  match l |> List.sort ~compare:Int.compare |> loop with
  | None -> failwith "Not found"
  | Some a -> a
;;

let%expect_test "given examples" =
  let data = [ 1721; 979; 366; 299; 675; 1456 ] in
  printf "%d" (part1 data);
  [%expect {| 514579 |}];
  printf "%d" (part2 data);
  [%expect {| 241861950 |}]
;;
