open Belt

module History = Set.Int

type handheld = Nop(int) | Acc(int) | Jmp(int)
type progress = {
  value: int,
  cursor: int,
  log: Set.Int.t,
}

let day8_input =
  Node.Fs.readFileAsUtf8Sync("input/day8.txt")->Js.String2.trim->Js.String2.split("\n")

let handheld = input =>
  input->Array.keepMap(line => {
    let split_text = line->Js.String2.trim->Js.String2.split(" ")
    let key = split_text->Array.get(0)
    let value = split_text->Array.get(1)->Option.flatMap(v => v->Int.fromString)
    switch (key, value) {
    | (Some(c), Some(v)) =>
      switch c {
      | "nop" => Some(Nop(v))
      | "acc" => Some(Acc(v))
      | "jmp" => Some(Jmp(v))
      | _ => None
      }
    | (_, _) => None
    }
  })

let p1Result = start => {
  let rec recursion = history => {
    let isOverlap = (new, before) => before.log->Set.Int.has(new.cursor)

    let nextState = history =>
      switch day8_input->handheld->Array.getExn(history.cursor) {
      | Nop(_) => {
          value: history.value,
          cursor: history.cursor + 1,
          log: history.log->Set.Int.add(history.cursor),
        }
      | Acc(y) => {
          value: history.value + y,
          cursor: history.cursor + 1,
          log: history.log->Set.Int.add(history.cursor),
        }
      | Jmp(y) => {
          value: history.value,
          cursor: history.cursor + y,
          log: history.log->Set.Int.add(history.cursor),
        }
      }

    let progressData = history->nextState
    progressData->isOverlap(history) ? history.value : recursion(progressData)
  }

  recursion(start)
}

p1Result({value: 0, cursor: 0, log: Set.Int.empty})->Js.log

let p2Result = start => {
    // let input = ...
  let rec recursion = (value, cursor, log, is_change) => {
    if log->Set.Int.has(cursor) {
      None
    } else {
      day8_input
      ->handheld
      ->Array.get(cursor)
      ->Option.mapWithDefault(Some(value), x => {
        switch x {
        | Nop(y) => if is_change {
            recursion(value, cursor + 1, log->Set.Int.add(cursor), true)
          } else {
            // 처음, 정상 실행 => 재귀함수로 결국 여기에 리턴! 처음 시작과 끝 인지
            let returnValue = recursion(value, cursor + y, log->Set.Int.add(cursor), true)
            switch returnValue {
            | Some(v) => Some(v)
            | None => recursion(value, cursor + 1, log->Set.Int.add(cursor), false)
            }
          }
        | Acc(y) => recursion(value + y, cursor + 1, log->Set.Int.add(cursor), is_change)
        | Jmp(y) => if is_change {
            recursion(value, cursor + y, log->Set.Int.add(cursor), true)
          } else {
            let returnValue = recursion(value, cursor + 1, log->Set.Int.add(cursor), true)
            switch returnValue {
            | Some(v) => Some(v)
            | None => recursion(value, cursor + y, log->Set.Int.add(cursor), false)
            }
          }
        }
      })
    }
  }

  recursion(start.value, start.cursor, start.log, false)
}

p2Result({value: 0, cursor: 0, log: Set.Int.empty})->Js.log

/*
let rec run = (env, state) => {
    if (env.terminateFn(state)) {
        state
    } else {
        let newState = env.nextState(state)
        run(env, newState)
    }
}

type state
let terminateFn: state => boolean // 히스토리에 값이 있을 때
let nextStateFn: state => state // 다음으로 이동할 때, 히스토리에 추가하고...
let initialState: state // index 0

- env: terminateFn, nextStateFn = 안바뀐다(순수함수) => 환경
- state = 바뀐다 => 상태

let p1Run = run(terminateFn, nextStateFn)

p1Run(initialState);

p2
- 백트래킹
- p1 x N

type result = 
| Infinite
| Finite

let p1solve: program => result
let p2solve: array<program> => array<result> => result

// map = Functor

let p2solve = Belt.Array.map(p1solve)->Belt.Array.keep->Js.log

*/

// 1. 재귀적 위치를 다 기록하자 -> 첫번째 재귀 다음부터 위치의 시작은?
// 그리고 위치에 왔을 때 액셕을 다르게 해보자 -> a.b
// 2. 2번째 재귀일 때 이전 재귀 위치를 기록해서 다른 액션으로 돌리자
// -> 너무 봅잡해짐 더 혼동이.. 타입에 자식 타입을 넣고..
