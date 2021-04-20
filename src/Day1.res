open Belt

let day1_input =
  Node.Fs.readFileAsUtf8Sync("input/day1.txt")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.keepMap(x => x->Int.fromString)

let findInput = Set.Int.fromArray(day1_input)

let isExist = (targetNum, number1, number2) => {
  let number = targetNum - (number1 + number2)
  findInput->Set.Int.has(number)
}

//p1
day1_input
->Array.reduce(0, (acc, i) => {
  switch isExist(2020, i, 0) {
  | true => (2020 - i) * i
  | false => acc
  }
})

let existNumbes = (acc, targetNum, number1, number2) => {
  switch isExist(targetNum, number1, number2) {
  | true => {
      Set.Int.empty
      ->Set.Int.add(targetNum - (number1 + number2))
      ->Set.Int.add(number1)
      ->Set.Int.add(number2)
    }
  | false => acc->Set.Int.add(number2)
  }
}

let getNumbers = (input, target, filter, number) => {
  input->Array.reduce(Set.Int.empty, (acc, i) => {
    switch acc->Set.Int.toArray->Garter.Math.sum_int == target {
    | true => acc
    | false =>
      switch filter->Set.Int.has(i) {
      | true => acc->Set.Int.add(i)
      | false => existNumbes(acc, target, number, i)
      }
    }
  })
}

day1_input
->Array.reduce(Set.Int.empty, (acc, i) => {
  switch acc->Set.Int.toArray->Garter.Math.sum_int == 2020 {
  | true => acc
  | false => {
      let value = day1_input->getNumbers(2020, acc->Set.Int.add(i), i)
      value->Set.Int.toArray->Garter.Math.sum_int == 2020 ? acc : acc->Set.Int.add(i)
    }
  }
})
->Js.log
