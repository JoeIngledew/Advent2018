open System
open System.Text.RegularExpressions

type Record =
| Begin of DateTime * int
| Sleep of DateTime
| Wake of DateTime

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseRecord s =
    let dt = 
        match s with
        | Match @"\[([0-9-: ]+)\] Guard \#([0-9]+)" [date; id] -> DateTime.Parse(date),(id |> int)
        | Match @"\[([0-9-: ]+)\]" [date] -> DateTime.Parse(date), -1
        | _ -> failwith "Can's parse the date"
    if s.Contains("asleep") then 
        Sleep((fst dt))
    elif s.Contains("wakes") then 
        Wake((fst dt))
    else Begin(dt)
              

let extractResult (a,b,c) = a
let run f =
    System.IO.File.ReadAllLines(f)
    |> Array.toList
    |> List.map parseRecord
    |> List.sortBy (fun r -> 
        match r with 
        | Begin (a,_) -> a
        | Sleep x -> x
        | Wake x -> x)
    |> List.fold (fun ((sts: Map<int, float>),id,(bgt: DateTime option)) r ->
        match r with
        | Begin (dt, i) -> 
            (sts, i, None)
        | Sleep dt ->
            sts, id, Some(dt)
        | Wake dt ->
            let sleepTime = (dt - bgt.Value).TotalMinutes
            if (sts |> Map.containsKey(id)) then
                let total = sleepTime + sts.[id]
                let filtered = sts |> Map.remove id |> Map.add id total
                filtered,id,None
            else 
                let newMap = sts |> Map.add id sleepTime
                newMap,id,None) (Map.empty, -1, None)
    |> extractResult
    |> Map.toList
    |> List.maxBy snd // tod get the minute the most 