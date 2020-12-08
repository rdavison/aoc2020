type t = [ `Empty | `Tree ] array array [@@deriving sexp]

let parse_row row =
  row
  |> String.to_array
  |> Array.map ~f:(function
         | '.' -> `Empty
         | '#' -> `Tree
         | c -> Printf.failwithf "Parse error: Unknown char: %c" c ())
;;

let of_string x =
  x
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
         match String.strip line with
         | "" -> None
         | line -> Some (parse_row line))
  |> List.to_array
;;

let width t = Array.length t.(0)
let height t = Array.length t

let get (t : t) ~row ~col =
  let col = col % width t in
  match row < height t with
  | true -> Some t.(row).(col)
  | false -> None
;;

let part1 t =
  let d_row = 1 in
  let d_col = 3 in
  let rec loop row col acc =
    match get t ~row ~col with
    | None -> acc
    | Some kind ->
      let loop acc = loop (row + d_row) (col + d_col) acc in
      (match kind with
      | `Tree -> loop (acc + 1)
      | `Empty -> loop acc)
  in
  loop 0 0 0
;;

let%expect_test "of_string example" =
  let data =
    String.concat
      ~sep:"\n"
      [ "..##......."
      ; "#...#...#.."
      ; ".#....#..#."
      ; "..#.#...#.#"
      ; ".#...##..#."
      ; "..#.##....."
      ; ".#.#.#....#"
      ; ".#........#"
      ; "#.##...#..."
      ; "#...##....#"
      ; ".#..#...#.#"
      ]
  in
  let t = of_string data in
  print_s (sexp_of_t t);
  [%expect
    {|
    ((Empty Empty Tree Tree Empty Empty Empty Empty Empty Empty Empty)
     (Tree Empty Empty Empty Tree Empty Empty Empty Tree Empty Empty)
     (Empty Tree Empty Empty Empty Empty Tree Empty Empty Tree Empty)
     (Empty Empty Tree Empty Tree Empty Empty Empty Tree Empty Tree)
     (Empty Tree Empty Empty Empty Tree Tree Empty Empty Tree Empty)
     (Empty Empty Tree Empty Tree Tree Empty Empty Empty Empty Empty)
     (Empty Tree Empty Tree Empty Tree Empty Empty Empty Empty Tree)
     (Empty Tree Empty Empty Empty Empty Empty Empty Empty Empty Tree)
     (Tree Empty Tree Tree Empty Empty Empty Tree Empty Empty Empty)
     (Tree Empty Empty Empty Tree Tree Empty Empty Empty Empty Tree)
     (Empty Tree Empty Empty Tree Empty Empty Empty Tree Empty Tree)) |}];
  printf "%d" (part1 t);
  [%expect {| 7 |}]
;;
