open Belt

module CoordCmp = Id.MakeComparable({
  type t = (int, int)  
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let day3_input =
  Node.Fs.readFileAsUtf8Sync("input/day3.txt")->Js.String2.trim->Js.String2.split("\n")

let maxHeight = Array.length

let makePositionList = ((step_x, step_y)) =>
  day3_input->Array.reduceWithIndex(list{}, (acc, line, y) => {
    let width = line->Js.String2.split("")->Array.length
    let x = mod(y * step_x, width)
    let new_y = mod(y * step_y, maxHeight(day3_input))
    acc->List.add((x, new_y))
  })

let treeMap = day3_input->Array.reduceWithIndex(Map.make(~id=module(CoordCmp)), (acc, line, y) =>
  line
  ->Js.String2.split("")
  ->Array.reduceWithIndex(acc, (acc1, element, x) => {
    let value = element === "#" ? 1 : 0
    acc1->Map.set((x, y), value)
  })
)

// https://github.com/green-labs/garter/blob/main/src/Garter_Math.res
let treeCount = step =>
  step
  ->makePositionList
  ->List.map(treeMap->Map.getExn)      
  ->List.reduce(0, \"+")  


// p1
treeCount((3, 1))
->Js.log

// // p2
list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}
->List.map(treeCount)
->List.map(Int.toFloat)
->List.reduce(1.0, \"*.")
->Js.log
