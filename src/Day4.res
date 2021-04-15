open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/day4.txt")->Js.String2.trim
->Js.String2.split("\n\n")
->Array.map(s => s->Js.String2.replaceByRe(%re("/\\n/g"), " "))

type passport = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<string>
}

let convertSplit = input => 
    input->Js.String2.split(" ")->Array.map(x => {
        x->Js.String2.split(":")        
    })

let convertValue = input =>
    input->Array.reduce(Map.String.empty, (acc, s) =>{
        switch s {
        | [key, value] => acc->Belt.Map.String.set(key, value)
        | _ => acc
        }
    })

let hasPassportKey = input => 
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]->Array.every(k => input->Map.String.has(k))



let matchedStrings = (str, re) => {
  re
  ->Js.Re.exec_(str)
  ->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(Js.Nullable.toOption)
}

//case 문이 없는지

// parseInt("1") = 1 Int or NaN
// validateInt(1);
let validatePassportFormat = (x: passport): bool => {
  x.byr >= 1920 &&
  x.byr <= 2002 &&
  x.iyr >= 2010 &&
  x.iyr <= 2020 &&
  x.eyr >= 2020 &&
  x.eyr <= 2030 &&
  switch x.hgt->matchedStrings(%re("/^(\d+)(cm|in)$/")) {
  | [Some(height), Some(heightUnit)] => {
      let heightInt = height->Belt.Int.fromString->Belt.Option.getWithDefault(0)
      if heightUnit == "cm" {
        heightInt >= 150 && heightInt <= 193
      } else if heightUnit == "in" {
        heightInt >= 59 && heightInt <= 76
      } else {
        false
      }
    }
  | _ => false
  } &&
  %re("/^#[\da-f]{6}$/")->Js.Re.test_(x.hcl) &&
  %re("/^amb|blu|brn|gry|grn|hzl|oth$/")->Js.Re.test_(x.ecl) &&
  %re("/^\d{9}$/")->Js.Re.test_(x.pid)
}

// let parsePassport: Belt.Map.String.t => passport

// 이 함수만 output 타입에 passport가 있다면,
// 프로그램 전체에서는 이 함수를 통과하지 않고서는 passport 타입이 생성되지 않음.
// 프로그램 전체에서 passport를 쓰면 안전함. 왜냐? 이 함수를 통과한 친구만 들어갈 수 있기 때문에
// parse = parse + validate => type이 나오는 것
// https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
let parsePassport: Belt.Map.String.t<string> => option<passport> = s => {
    let passport = {
        byr: s->Belt.Map.String.getExn("byr")->Int.fromString->Belt.Option.getWithDefault(0),
        iyr: s->Belt.Map.String.getExn("iyr")->Int.fromString->Belt.Option.getWithDefault(0),
        eyr: s->Belt.Map.String.getExn("eyr")->Int.fromString->Belt.Option.getWithDefault(0),
        hgt: s->Belt.Map.String.getExn("hgt"),
        hcl: s->Belt.Map.String.getExn("hcl"),
        ecl: s->Belt.Map.String.getExn("ecl"),
        pid: s->Belt.Map.String.getExn("pid"),
        cid: s->Belt.Map.String.get("cid")
    }
    switch validatePassportFormat(passport) {
    | true => Some(passport)
    | false => None
    }
 }

input
->Array.map(convertSplit)
->Array.map(convertValue)
->Array.keep(hasPassportKey)
->Array.keepMap(parsePassport)
->Array.length
->Js.log

 /*
->Array.map(mapFn) // array<Belt.Map.String.t<string>> => array<passport> : 이 passport는 틀릴 수도 있음.
->Array.map(validFn) // array<passport> => array<bool> : [true, false, true ...]
->Array.keep(x => x) // [true, true, true..] ...

https://one-day.dev/post/005-greenlabs/

 */
