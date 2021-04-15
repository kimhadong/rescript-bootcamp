open Belt

let day5_input =
  Node.Fs.readFileAsUtf8Sync("input/day5.txt")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(seat_txt => seat_txt->Js.String2.split(""))

let find_number = (input, start_position, y_check_txt) => {
  let (x, _) =
    input->Array.reduce(start_position, ((x, y), row) =>
      row === y_check_txt ? (x, (x + y) / 2) : ((x + y) / 2 + 1, y)
    )
  x
}

let findSeat = seat_array => (
  seat_array->Array.slice(~offset=0, ~len=7)->find_number((0, 127), "F"),
  seat_array->Array.sliceToEnd(7)->find_number((0, 7), "L"),
)

let make_seatId = ((x, y)) => x * 8 + y

let getSeatMyId = ((odd, even)) => {  
  let (left, right) = odd->Js.Math.minMany_int > even->Js.Math.minMany_int
      ? (even->Array.map(i => i + 1), odd)
      : (odd->Array.map(i => i + 1), even)
  
  let (left_set, right_set) =
    left->Array.length > right->Array.length
      ? (Belt.Set.Int.fromArray(left), Belt.Set.Int.fromArray(right))
      : (Belt.Set.Int.fromArray(right), Belt.Set.Int.fromArray(left))
  
  let diff_value = Belt.Set.Int.diff(left_set, right_set)->Set.Int.maximum->Option.getWithDefault(0) // maxUndefined          
  odd->List.fromArray->List.has(diff_value, (a, b) => a == b) 
    ? diff_value-2
    : diff_value-1
}

//p1
day5_input
->Array.map(findSeat)
->Array.map(make_seatId)
->Js.Math.maxMany_int
->Js.log

// p2
day5_input
->Array.map(findSeat)
->Array.map(make_seatId)
->SortArray.stableSortBy(Pervasives.compare)
->Array.partition(x => mod(x, 2) != 0)
->getSeatMyId
->Js.log


