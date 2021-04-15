open Belt

type handheld = Nop(int) | Acc(int) | Jmp(int)
type progress = {
  value: int,
  cursor: int,
  log: Set.Int.t,
}

// type p2progress = {
//   value2: int,
//   cursor2: int,
//   log2: Set.Int.t,
//   beforeAction: progress
// }

let day8_input =
  Node.Fs.readFileAsUtf8Sync("input/day8.txt")->Js.String2.trim->Js.String2.split("\n")

let handheld = input =>
  input
  ->Array.keepMap(line => {
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
  ->Array.reduceWithIndex(Map.Int.empty, (acc, i, idx) => {
    acc->Map.Int.set(idx, i)
  })
// Map.Int.getExt(10) == Array.getExt(10)

let p1Result = (ponter, start) => {
    let rec recursion = (witch, history) => { 
    
        let progressRecursion = (history, value, cursor, log) => {    
            history.log->Set.Int.has(cursor) 
            ? history.value
            : recursion(cursor, {value: value, cursor: cursor, log: log})
        }

        switch  day8_input->handheld->Map.Int.getExn(witch){
        | Nop(_) => {
            history->progressRecursion(history.value, witch + 1, history.log->Set.Int.add(witch))
        }
        | Acc(y) => {
            history->progressRecursion(history.value + y, witch + 1, history.log->Set.Int.add(witch))
        }
        | Jmp(y) => {
            history->progressRecursion(history.value, witch + y, history.log->Set.Int.add(witch))
        }
        }
    }

    recursion(ponter, start)
}

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

*/

p1Result(0, {value: 0, cursor: 1, log: Set.Int.empty})->Js.log


//1824
//1673
//1174
let p3Result = (start) => {
    let rec recursion = (value, cursor, log, is_change) => {        
        if(log->Set.Int.has(cursor)){
            None
        } else {
            // 값을 뽑았을 때 없는 cursor
            // 그게 정답
            day8_input->handheld->Map.Int.get(cursor)->Option.mapWithDefault(Some(value), x => {
                switch x {
                    | Nop(y) => {
                        if is_change {
                            recursion(value, cursor + 1, log->Set.Int.add(cursor), true)
                        } else {
                            // 처음, 정상 실행 => 재귀함수로 결국 여기에 리턴! 처음 시작과 끝 인지
                            let returnValue = recursion(value, cursor + y, log->Set.Int.add(cursor), true)
                            switch returnValue {
                            | Some(v) => Some(v)
                            | None => recursion(value, cursor + 1, log->Set.Int.add(cursor), false)
                            }   
                        }                                                
                    }
                    | Acc(y) => {
                        recursion(value + y, cursor + 1, log->Set.Int.add(cursor), is_change)
                    }
                    | Jmp(y) => {   
                        if is_change {
                            recursion(value, cursor + y, log->Set.Int.add(cursor), true)
                        } else {
                            let returnValue = recursion(value, cursor + 1, log->Set.Int.add(cursor), true)
                            switch returnValue {
                            | Some(v) => Some(v)
                            | None => recursion(value, cursor+y, log->Set.Int.add(cursor), false)
                            }
                        }                                  
                    }                
                }
            })
        }        
    }

    recursion(start.value, start.cursor, start.log, false)
}

p3Result({value: 0, cursor: 0, log: Set.Int.empty})->Js.log





// let p2Result = (ponter, start) => {
//     //let checkPonts = testResult(0, {value: 0, cursor: 1, log: Set.Int.empty})

//     let rec recursion = (witch, history) => {        
//         //Js.log(history.log->Set.Int.toArray->Array.length)
//         //Js.log(witch)
//         //중간에 값을 리턴하는 함수는?
//         //2135  
//         let progressRecursion = (history, value, cursor, log, beforeAction) => {                
//             history.log2->Set.Int.has(cursor) 
//             ? history.value2
//             : day8_input->handheld->Map.Int.size == cursor ? history.value2 : recursion(cursor, {value2: value, cursor2: cursor, log2: log, beforeAction: beforeAction})
//         }
        
//         //이전에 Nop->Jmp 혹은 Jmp->Nop로 바꾼 지점으로 가서 바꾸지말고 그대로 실행            
//         if(history.log->Set.Int.has(witch)){
//            None
//         } else {
//             switch  day8_input->handheld->Map.Int.getExn(witch){
//             | Nop(y) => {            
//                 if(history.log2->Set.Int.has(witch + 1)){                                
//                     Js.log(history.beforeAction.cursor)
//                     //이전에 처리 조회
//                     if history.beforeAction.cursor == 0 {
//                         history->progressRecursion(history.value2, witch + y, history.log2->Set.Int.add(witch), {value: history.value2, cursor: history.cursor2, log: history.log2})                                
//                     } else if history.beforeAction.cursor == -1 {
//                         history->progressRecursion(history.value2, witch + 1, history.log2->Set.Int.add(witch), {value: 0, cursor: 0, log: Set.Int.empty})
//                     } else {                    
//                         //다시 중복으로 빠진 상황으로 이전 중복 값으로 되돌아 간다                    
//                         history->progressRecursion(history.beforeAction.value, history.beforeAction.cursor, history.beforeAction.log, {value: 0, cursor: -1, log: Set.Int.empty})
//                         //history->progressRecursion(history.value, witch + 1, history.log->Set.Int.add(witch), history.beforeAction->Map.Int.set(witch, "jmp"), "none")
//                     }
//                 } else {         
//                     history->progressRecursion(history.value2, witch + 1, history.log2->Set.Int.add(witch), history.beforeAction)
//                 }
//             }
//             | Acc(y) => {
//                 if(history.log2->Set.Int.has(witch + 1)){                
//                     Js.log(history.beforeAction.cursor)
//                     //이전에 처리 조회
//                     if history.beforeAction.cursor == 0 {
//                         history->progressRecursion(history.value2 + y, witch + 1, history.log2->Set.Int.add(witch), {value: history.value2, cursor: history.cursor2, log: history.log2})
//                     } else if history.beforeAction.cursor == -1 {
//                         history->progressRecursion(history.value2 + y, witch + 1, history.log2->Set.Int.add(witch), {value: 0, cursor: 0, log: Set.Int.empty})
//                     } else {
//                         //다시 중복으로 빠진 상황으로 이전 중복 값으로 되돌아 간다                    
//                         history->progressRecursion(history.beforeAction.value, history.beforeAction.cursor, history.beforeAction.log, {value: 0, cursor: -1, log: Set.Int.empty})
//                         //history->progressRecursion(history.value, witch + 1, history.log->Set.Int.add(witch), history.beforeAction->Map.Int.set(witch, "jmp"), "none")
//                     }
//                 } else {
//                     history->progressRecursion(history.value2 + y, witch + 1, history.log2->Set.Int.add(witch), {value: 0, cursor: 0, log: Set.Int.empty})
//                 }
//                 //if(history.log2->Set.Int.has(witch + 1) && execAction == "change"){
//                     //history->progressRecursion(history.value2 + y, witch + 1, history.log2->Set.Int.add(witch), {value: 0, cursor: 0, log: Set.Int.empty}, "change")

//             }
//             | Jmp(y) => {
//                 //Js.log(history.beforeAction.cursor)
//                 if(history.log2->Set.Int.has(witch + y)){
//                     Js.log(history.beforeAction.cursor)
//                 //if(checkPonts->Set.Int.has(witch)){
//                     if history.beforeAction.cursor == 0 {
//                         history->progressRecursion(history.value2, witch + 1, history.log2->Set.Int.add(witch), {value: history.value2, cursor: history.cursor2, log: history.log2})
//                     } else if history.beforeAction.cursor == -1 {
//                         history->progressRecursion(history.value2, witch + y, history.log2->Set.Int.add(witch), {value: 0, cursor: 0, log: Set.Int.empty})                    
//                     } else {
//                         history->progressRecursion(history.beforeAction.value, history.beforeAction.cursor, history.beforeAction.log, {value: 0, cursor: -1, log: Set.Int.empty})                    
//                     }                
//                 } else {           
//                     history->progressRecursion(history.value2, witch + y, history.log2->Set.Int.add(witch), history.beforeAction)
//                 }
//             }        
//             }
//         }

        
//     }

//     recursion(ponter, start)
// }

// let run = tt => {
//     let log p1Result(0, {value: 0, cursor: 1, log: Set.Int.empty})
// }

//day8_input->Array.length->Js.log

//testResult(0, {value: 0, cursor: 1, log: Set.Int.empty})->Set.Int.size->Js.log


//p1Result(0, {value: 0, cursor: 1, log: Set.Int.empty})->Js.log

//day8_input->handheld->Map.Int.getExn(612)->Js.log
//day8_input->handheld->Map.Int.size->Js.log


// 1. 재귀적 위치를 다 기록하자 -> 첫번째 재귀 다음부터 위치의 시작은?
// 그리고 위치에 왔을 때 액셕을 다르게 해보자 -> a.b
// 2. 2번째 재귀일 때 이전 재귀 위치를 기록해서 다른 액션으로 돌리자
// -> 너무 봅잡해짐 더 혼동이.. 타입에 자식 타입을 넣고..
