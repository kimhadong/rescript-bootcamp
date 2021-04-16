open Belt

let day7_input =
  Node.Fs.readFileAsUtf8Sync("input/day7.txt")
  ->Js.String2.split(".\n")
  ->Array.map(i =>
    i
    ->Js.String2.replaceByRe(%re("/bags/g"), "bag")
    //->Js.String2.replaceByRe(%re("/[0-9]/g"), "")
    ->Js.String2.trim
    ->Js.String2.split(" contain ")
  )

let matchedStrings = (str, re) => {
  re
  ->Js.Re.exec_(str)
  ->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(Js.Nullable.toOption)
}

// inputMap: Map.String.t<Map.String.t<int>> : adjacency list의 notation
let inputMap = day7_input->Array.reduce(Map.String.empty, (acc, i) => {
  acc->Map.String.set(
    i->Array.getExn(0),
    i
    ->Array.getExn(1)
    ->Js.String2.split(",")
    ->Array.keepMap(i => {
      switch matchedStrings(i->Js.String2.trim, %re("/^(\d)\s([A-z]+\s[A-z]+\s[A-z]+)\.?/g")) {
      | [Some(count), Some(bag)] =>        
        Some((bag, count->Int.fromString->Option.getWithDefault(0)))
      | _ => None
      }
    })
    ->Map.String.fromArray
  )
})

let isExist = (input, findBag) => input->Map.String.has(findBag)

let rec findContainBag = (input, findBag, acc) => {
  input->Belt.Map.String.reduce(acc, (acc, k, v) => {
    if v->isExist(findBag) {
      input->findContainBag(k, acc->Belt.Set.String.add(k))
    } else {
      acc
    }
  })
}
findContainBag(inputMap, "shiny gold bag", Set.String.empty)->Set.String.size->Js.log

let rec p2findContainBag = (input, findBag, value) => {
  input
  ->Map.String.get(findBag)
  ->Option.map(i => {
    i->Belt.Map.String.reduce(value, (acc, k, v) => {
      //acc + value * input->p2findContainBag(k, v)
      acc + value * input->p2findContainBag(k, v)
    })
    //i->Map.String.reduce(value, (acc, k, v) => {
    // i == shiny gold bag map list
    // k -> 자식 가방 이름 재귀
    // v --> k가방의 갯수
    // value -> 누적된 개수
    // v + value * (v + value * (v + value * (v + value * (v + value * (v + value * (v)))))) - 1
    // v + value * => 재귀
    // 누적된 개수
    // 3 -> 9
    // 9 + 다음 키 포함 개수들
    //누적(기존) + 현재조회하고 있는 개수 * 하위포함개수
    //})
  })
  ->Option.getWithDefault(value)
}
(inputMap->p2findContainBag("shiny gold bag", 1) - 1)->Js.log





