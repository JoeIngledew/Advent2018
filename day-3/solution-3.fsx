open System.Text
open System.Text.RegularExpressions
open System.Collections
open System.Collections.Generic
let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Claim = {
    id: int
    leftEdge: int
    topEdge: int
    width: int
    height: int
} with 
    static member CreateFrom (s: string) =
        match s with
        | Match @"\#([0-9]+) \@ ([0-9]+)\,([0-9]+)\: ([0-9]+)x([0-9]+)" [ id; left; top; width; height ] ->
            { 
                id = id |> int;
                leftEdge = left |> int;
                topEdge = int top;
                width = int width;
                height = int height 
            }
        | _ -> failwith "broken"

    member t.CoveredCoords =
        let xs = [(t.leftEdge+1)..(t.leftEdge+t.width)]
        let ys = [(t.topEdge+1)..(t.topEdge+t.height)]
        xs 
        |> List.map (fun x -> ys |> List.map (fun y -> x,y))

let rec countRepeats (set: HashSet<'a>) (xs: 'a list) count =
    match xs with
    | x::xs ->
        let added = set.Add(x)
        if added then countRepeats set xs count
        else countRepeats set (xs |> List.filter (fun y -> y = x)) (count+1)
    | [] -> count    

let run f =
    let lines = System.IO.File.ReadAllLines(f) |> Array.toList
    let allClaims = lines |> List.map Claim.CreateFrom
    let claimsCovered = allClaims |> List.map (fun c -> c.CoveredCoords) |> List.concat |> List.concat
    claimsCovered |> List.groupBy (fun a -> a) |> List.filter (fun (k, v) -> v |> List.length > 1) |> List.length

// let findOverlap (points: (int * int) list) (others : (Claim * ((int*int) list)) list)=
//     let os = others |> List.map snd |> List.concat
//     let overlaps = 


// let findUncovered cs =
//     match cs with
//     | (c,xs)::ys ->
//         let overlaps = findOverlap xs ys


let run2 f = 
    let lines = System.IO.File.ReadAllLines(f) |> Array.toList
    let allClaims = lines |> List.map Claim.CreateFrom
    let claimsCovered = allClaims |> List.map (fun c -> (c.CoveredCoords |> List.concat |> List.map (fun o -> c.id,o))) |> List.concat
    let uncoveredById =
        claimsCovered 
        |> List.groupBy snd
        |> List.filter (fun (a, bs) -> bs |> List.length = 1)
        |> List.map snd
        |> List.concat
        |> List.groupBy fst
        |> List.map (fun (a,bs) -> a, bs |> List.map snd)
    let fullClaims =
        allClaims
        |> List.filter (fun x -> uncoveredById |> List.exists (fun (a,_) -> a = x.id))
        |> List.map (fun c -> c.id,c.CoveredCoords |> List.concat)  
        |> List.filter (fun (id,coords) -> 
            let matchingUncovered = uncoveredById |> List.filter (fun (a,_) -> a = id) |> List.exactlyOne
            (matchingUncovered |> snd |> List.length) = (coords |> List.length))   
    fullClaims
    //uncoveredById

    // |> List.fold (fun p n -> 
    //     if (p |> List.exists (fun x -> x = n)) then p else n::p) []
    // |> List.length    