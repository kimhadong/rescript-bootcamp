open Belt

let day6_input =
  Node.Fs.readFileAsUtf8Sync("input/day6.txt")->Js.String2.trim->Js.String2.split("\n\n")
  ->Array.map(input =>
    input->Js.String2.split("\n")
    ->Array.map(i => i->Js.String2.split(""))
    ->Array.map(x => x->Set.String.fromArray)
  )

let unionFilter = input => input->Array.reduce( Set.String.empty, (acc, value) => acc->Set.String.union(value))
let intersectFilter = input => input->Garter.Array.reduce1((. acc, value) => acc->Belt.Set.String.intersect(value))

let getResult = (filter) =>
  day6_input
  ->Array.map(filter)
  ->Array.map(value=>value->Set.String.size)  
  ->Array.reduce(0, \"+")
  

getResult(unionFilter)->Js.log
getResult(intersectFilter)->Js.log

// 함수 내부가 pt1, pt2 여부를 체크하는 것은 anti-pattern!