open System.Text.RegularExpressions

// unsolved
let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Link = {
    ForStep: string
    PreReq: string
}
let parseLine s =
    match s with 
    | Match @"Step ([A-Z]) must be finished before step ([A-Z])" [prereq;forStep] -> { ForStep = forStep; PreReq = prereq }
    | _ -> failwith "Unable to parse"

let produceGraph (links: Link list) =

