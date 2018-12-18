// UNSOLVED


open System
open System.Text.RegularExpressions

type GuardId = int
type Hour = int
type Minute = int

type Time = {
    Hour: int
    Minute: int
}
    with
        static member GetMinutes (start: Time) (finish: Time) = 
            let day t = if t.Hour = 0 then 2 else 1
            let dts,dtf = (new DateTime(1, 1, (day start), start.Hour, start.Minute, 0)), (new DateTime(1, 1, (day finish), finish.Hour, finish.Minute, 0))
            let rec loop (mins: int list) (dtCurr: DateTime) = 
                if dtCurr >= dtf then mins
                else
                    let minute = dtCurr.Minute |> int
                    let dtNext = dtCurr.AddMinutes((float 1))
                    loop (minute::mins) dtNext
            loop [] dts                            
        static member Compare (dt1: Time) (dt2: Time) =
            match dt1.Hour,dt2.Hour with
            | 0 ,23 -> 1
            | 23, 0 -> -1
            | _     -> 
                if dt1.Minute > dt2.Minute then 1 elif dt2.Minute > dt1.Minute then -1 else 0       

type Record =
| Begin of DateTime * int
| Sleep of DateTime
| Wake of DateTime

type Record2 =
    | BeginHourMin of Time * GuardId
    | SleepHourMin of Time
    | WakeHourMin of Time
        with
            member x.GetTime = 
                match x with 
                | BeginHourMin(t,_) -> t
                | SleepHourMin(t) -> t
                | WakeHourMin(t) -> t
            static member CompareTime (r1: Record2) (r2: Record2) =
                Time.Compare (r1.GetTime) (r2.GetTime)

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parse2 s =
    match s with
    | Match @"\[[0-9]{4}\-[0-9]{2}\-[0-9]{2} ([0-9]{2})\:([0-9]{2})\] Guard \#([0-9]+)" [hour;minute;id] -> BeginHourMin ({ Hour = int hour; Minute= int minute}, int id)
    | Match @"\[[0-9]{4}\-[0-9]{2}\-[0-9]{2} ([0-9]{2})\:([0-9]{2})\]" [hour; minute] -> 
        let t = { Hour = int hour; Minute = int minute }
        if s.Contains("asleep") then SleepHourMin (t)
        else WakeHourMin (t)
    | _ -> failwith "Unparseable"    

let addOrReplace (m: Map<'a,'b>) (key: 'a) (value: 'b) (valConcat: 'b -> 'b -> 'b) =
    if m |> Map.containsKey key then
        let newVal = valConcat value m.[key]
        m |> Map.remove key |> Map.add key newVal
    else m |> Map.add key value

let listValConcat l1 l2 = l1 @ l2

let trimTripleTail (a,b,c) = a

type FoldType = Map<int, int list> * int * Time 
let getMap (x: FoldType) =
    let m,_,_ = x
    m

let getMinutesPerGuard (x: FoldType) (next: Record2): FoldType =
    let cMap,cId,sleepStart = x
    match next with
    | BeginHourMin (t, id) -> cMap,id,sleepStart
    | SleepHourMin (t) -> cMap,cId,t
    | WakeHourMin (t) -> 
        let minutes = Time.GetMinutes sleepStart t
        (addOrReplace cMap cId minutes listValConcat), cId, sleepStart

type ResType = { Id: int; MaxMin: int } with member x.Write = sprintf "ID: %d, Minute most often: %d" x.Id x.MaxMin

let toIdMaxMin (a,mins) =
    let mostOftenMin =
        mins 
        |> List.fold (fun (prev: Map<int,int>) (next: int) ->
            addOrReplace prev next 1 (fun a b -> a + b)) Map.empty
        |> Map.toList
        |> List.maxBy snd
        |> fst
    { Id = a; MaxMin= mostOftenMin}        


let run3 f =
    let startPos: FoldType = Map.empty, -1, {Hour = -1; Minute = -1}
    System.IO.File.ReadAllLines(f)
    |> Array.toList
    |> List.map parse2
    |> List.sortWith Record2.CompareTime
    |> List.fold getMinutesPerGuard startPos
    |> getMap
    |> Map.toList
    |> List.maxBy (fun (a,b) -> b |> List.length)
    |> toIdMaxMin
    |> printfn "%A"
    // |> List.fold (fun ((cMap: Map<int,(int list)>),(cId: int),(sleepStart: Time)) (next: Record2) -> 
    //     match next with
    //     | BeginHourMin (t, id) -> cMap,id,sleepStart
    //     | SleepHourMin (t) -> cMap,cId,t
    //     | WakeHourMin (t) -> 
    //         let minutes = Time.GetMinutes sleepStart t
    //         (addOrReplace cMap cId minutes listValConcat), cId, sleepStart
    //     ) (Map.empty, -1, {Hour=-1;Minute=-1})
    // |> trimTripleTail
    // |> Map.toList
    // |> List.maxBy (fun (a,b) -> b |> List.length)     



let parseRecord s =
    match s with
    | Match @"\[([0-9-: ]+)\] Guard \#([0-9]+)" [date; id] -> Begin (DateTime.Parse(date),(id |> int))
    | Match @"\[([0-9-: ]+)\]" [date] ->
        if s.Contains("asleep") then Sleep (DateTime.Parse(date))
        else Wake (DateTime.Parse(date))
    | _ -> failwith "Can's parse the date"
              

let extractResult (a,b,c) = a

let getMinutesBetween (dt1: DateTime) (dt2: DateTime) total =
    let minute = dt1.Minute
    [ for i in minute..(minute+total) do yield if i > 59 then (i-60) else i ]

type ResultFormat = {
    GuardId: int
    TotalMinutes: int
    MostOftenMin: int
} with member x.Pretty = sprintf "Total: %d, most often on minute %d" x.TotalMinutes x.MostOftenMin

let parseResult (res: (int * (int * (int list)))) =
    let id,a = res
    let total,mins = a
    {
        GuardId = id
        TotalMinutes = total
        MostOftenMin = mins |> List.countBy (fun item -> item) |> List.fold (fun (cp,lst) (item,c) -> if c > cp then (c,[item]) elif c = cp then (c, item::lst) else (cp,lst)) (0, []) |> snd |> List.take 1 |> List.exactlyOne
    }

let run f =
    System.IO.File.ReadAllLines(f)
    |> Array.toList
    |> List.map parseRecord
    |> List.sortBy (fun r -> 
        match r with 
        | Begin (a,_) -> a
        | Sleep x -> x
        | Wake x -> x)
    |> List.fold (fun ((sts: Map<int, (int * (int list))>),id,(bgt: DateTime option)) r ->
        match r with
        | Begin (dt, i) -> 
            (sts, i, None)
        | Sleep dt ->
            sts, id, Some(dt)
        | Wake dt ->
            let sleepTime = (dt - bgt.Value).TotalMinutes |> int
            let minutesEncapsulated = getMinutesBetween bgt.Value dt sleepTime
            if (sts |> Map.containsKey(id)) then
                let total = sleepTime + (fst sts.[id])
                let filtered = sts |> Map.remove id |> Map.add id (total, ((snd sts.[id]) @ minutesEncapsulated))
                filtered,id,None
            else 
                let newMap = sts |> Map.add id (sleepTime, minutesEncapsulated)
                newMap,id,None) (Map.empty, -1, None)
    |> extractResult
    |> Map.toList
    |> List.maxBy (fun x -> x |> snd |> fst) // tod get the minute the most 
    |> parseResult

let run2 f =
    let orderedRecords = 
        System.IO.File.ReadAllLines(f)
        |> Array.toList
        |> List.map parseRecord
        |> List.sortBy (fun r -> 
            match r with 
            | Begin (a,_) -> a
            | Sleep x -> x
            | Wake x -> x)
    let rec loop rs (sleeps: Map<int,int>) (minutesSlept: Map<int, int list>) (prev: Record) currId =
        match rs with 
        | [] -> sleeps, minutesSlept
        | x::xs -> 
            match x with
            | Wake dt -> 
                match prev with 
                | Sleep y -> 
                    let timeSlept = (y - dt).TotalMinutes |> int
                    let minutes = [y.Minute..dt.Minute]

                    match ((sleeps |> Map.containsKey currId), (minutesSlept |> Map.containsKey currId)) with
                    | true, true -> 
                        let newTimeSlept = timeSlept + sleeps.[currId]
                        let newMinutes = minutes @ minutesSlept.[currId]
                        loop xs (sleeps |> Map.remove currId |> Map.add currId newTimeSlept) (minutesSlept |> Map.remove currId |> Map.add currId newMinutes) x currId
                    | true, false ->
                        let newTimeSlept = timeSlept + sleeps.[currId]
                        loop xs (sleeps |> Map.remove currId |> Map.add currId newTimeSlept) (minutesSlept |> Map.add currId minutes) x currId
                    | false, true -> 
                        let newMinutes = minutes @ minutesSlept.[currId]
                        loop xs (sleeps |> Map.add currId timeSlept) (minutesSlept |> Map.remove currId |> Map.add currId newMinutes) x currId
                    | _ ->        
                        loop xs (sleeps |> Map.add currId timeSlept) (minutesSlept |> Map.add currId minutes) x currId                                                      
                | _ -> failwith "This doesn't make sense!"                
            | Begin (_,id) -> loop xs sleeps minutesSlept x id         
            | _ -> loop xs sleeps minutesSlept x currId
    let allSleeps, allMinutesSlept = loop orderedRecords Map.empty Map.empty (Wake (DateTime.Now)) -1
    let sleepyGuardId = allSleeps |> Map.toList |> List.maxBy snd |> fst
    let sleepyGuardMins = allMinutesSlept.[sleepyGuardId]
    printfn "slept %A" sleepyGuardMins 
    let mostOftenMin =
        sleepyGuardMins 
        |> List.fold (fun p n -> 
            if (p |> Map.containsKey n) then
                let res = p.[n] + 1
                p |> Map.remove n |> Map.add n res
            else p |> Map.add n 1) Map.empty
        |> Map.toList
        |> List.maxBy snd
        |> fst        
    printfn "ID: %d, often min: %d" sleepyGuardId mostOftenMin