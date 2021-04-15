open Belt

let day2_input = Node.Fs.readFileAsUtf8Sync("input/day2.txt")
->Js.String2.trim
->Js.String2.split("\n")
->Array.map(i => {i->Js.String2.split(" ")})

type rule = (int, int)
type password = {
  rule: rule,
  char: string,
  password: string,
}

let parseRule = x => {
  x
  ->Js.String2.split("-")
  ->Belt.Array.map(x => x->Belt.Int.fromString)
}

// array<option<int>>
// [Some(1), Some(2), Some(3)]
// [Some(1), Some(2), None, Some(3)]

// option<array<int>>
// Some([1,2,3])
// None

let convertRule = value => {
  let convertValue =
    value
    ->Js.String2.split("-")
    ->Belt.Array.map(x => x->Belt.Int.fromString)
  switch convertValue {
  | [Some(x), Some(y)] => Some(x, y)
  | _ => None
  }
}

let convertPassword = value => {
  switch value {
  | [rule, char, password] =>
    convertRule(rule)->Option.map(rule =>
      {
        rule,
        char: char->Js.String2.get(0),
        password: password,
      }
    )
  | _ => None
  }
}

let commonInput = input => input->Array.keepMap(convertPassword)

//P1
commonInput(day2_input)
->Array.keep(x => {
  let (min, max) = x.rule
  let charCount =
    x.password
    ->Js.String2.split("")
    ->Array.reduce(0, (acc, i) => {
      i == x.char ? acc + 1 : acc
    })
  min <= charCount && max >= charCount
})
->Array.length
->Js.log

//P2
commonInput(day2_input)
->Array.keep(x => {
  let (min, max) = x.rule
  let matchCount = [min, max]->Array.reduce(0, (acc, i) => {
    x.char == x.password->Js.String2.get(i - 1) ? acc + 1 : acc
  })
  matchCount == 1
})
->Array.length
->Js.log



// array<string> 튜플로 변경 -> 더 간단한 방법이?
// 자료구조 변경의 중점
// 타입을 선언해서 사용하는 지?
