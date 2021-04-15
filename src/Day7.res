open Belt

let day7_input = Node.Fs.readFileAsUtf8Sync("input/day7.txt")
->Js.String2.split(".\n")
->Array.map(i => 
    i->Js.String2.replaceByRe(%re("/bags/g"), "bag")
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
let inputMap = day7_input
->Array.reduce(Map.String.empty, (acc, i) => {    
    acc->Map.String.set(
        i->Array.getExn(0), 
        i->Array.getExn(1)->Js.String2.split(",")
        ->Array.map(i => {
           matchedStrings(i, %re("/^(\d)\s([A-z]+\s[A-z]+\s[A-z]+)\.?/g"))           
        })
    )
})

let rec isExist = (map, key, target, count) => {
    //Js.log(key)
    let containBag = map->Map.String.get(key)->Option.getWithDefault([])
    //Js.log(containBag)
    switch Belt.Set.String.fromArray(containBag)->Set.String.has(target) {
    | true => {
        containBag->Array.reduce(count, (acc, i) => {
            let existCount = map->isExist(i, key, acc)
            existCount > 0 ? acc + existCount : acc
        })
    }
    | false => 0
    }
}

//let [x, y] = matchedStrings("2 dull fuchsia bags", %re("/^(\d)\s([A-z]+\s[A-z]+\s[A-z]+)\.?/g"))
//inputMap->Js.log


// let matchedStrings = (str, re) => {
//   let result = re->Js.Re.exec_(str)
//   //Js.log(result)
//   switch result {
//   | Some(r) => {
//       Js.log(Js.Re.captures(r))
//       let (Some(x), Some(y)) = Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x))
      
//     // r->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
//     // ->Belt.Array.sliceToEnd(1)
//     // ->Belt.Array.map(Js.Nullable.toOption)
//     (Some(x), Some(y))
//   }
//   | None => (Some(0), Some(0))
//   }

  
// }


matchedStrings("1 no other bags", %re("/^(\d)\s([A-z]+\s[A-z]+\s[A-z]+)\.?/g"))->Js.log



//inputMap->Js.log
// day7_input
// ->Array.map(i => isExist(inputMap, i->Array.getExn(0), "shiny gold bag", 1))
// ->Array.reduce(0, \"+")
// ->Js.log

//let re = Js.Re.fromString("([a-z]+)");


//문자열 비교와 값을 가져


// let re = Js.Re.fromString("/[0-9]/g");
// let result = re->Js.Re.exec_("2 dull fuchsia bags");
// let captures = Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x);
// //let captures = Js.Re.captures(result)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x);
// switch (result) {
// 	| Some(r) =>
// 		// captures 의 result 는 array<Js.nullable<string>>
// 		// option 으로 변환 및 None 필터링(선택)
// 		let captures = Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(x => x);
// 		captures[1]; // index 0은 전체, 1부터 subset 시작
// 		// let a = captures[1]->Belt.Int.fromString->Belt.Option.getExn; // int 로 변환하기 예제
// 	| None => raise(Not_found)
// }->Js.log