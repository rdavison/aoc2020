let day1 ~dir =
  Stdlib.Filename.concat dir "input"
  |> In_channel.read_all
  |> Day1.of_string
  |> Day1.run
  |> printf "%d"
;;
