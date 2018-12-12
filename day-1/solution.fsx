open System
open System.IO
open System.Collections.Generic
open System.Collections
let parseInputLine (s: string) = 
    let num = s.[1..] |> string |> int
    match s.[0] with
    | '-' -> 0 - num
    | _ ->  num

let readLines x = File.ReadAllLines(x)

let run f =
    f
    |> readLines
    |> Array.toList
    |> List.map parseInputLine
    //|> List.map (fun (s : string) -> s.ToCharArray())
    |> List.fold (fun a b -> a + b) 0


let extractResult a =
    let _,y,z = a
    y,z

let repeatTilDone instructions =
    let nums = 
        seq { while true do yield! instructions }
        |> Seq.scan (fun a x -> a + x) 0
    nums    
    |> Seq.mapi (fun ix i -> i,ix)
    |> Seq.skip 1
    |> Seq.find (fun i -> Seq.exists (fun j -> j = (fst i)) (Seq.take ((snd i)-1) nums))


type Either<'a, 'b> =
| Left of 'a
| Right of 'b

let rec createSet (xs: int list) (set: Set<int>) =
    match xs with
    | x::t -> if Set.exists (fun a -> a = x) set then Left(x) else createSet t (set.Add(x))
    | [] -> Right(set)

let getNextSet (set: HashSet<'a>) (next: 'a) =
    set.Add(next)

let findResult xs final =
    let s = new HashSet<int>()
    let mutable curr = 0
    Seq.initInfinite (fun i -> xs |> Seq.map (fun x -> x + (final * (i))))
    |> Seq.concat
    |> Seq.takeWhile (fun x ->
        printfn "Adding %d" x 
        curr <- x
        s.Add(x))
    |> Seq.last
    curr 

let runPartTwo f =
    let instructions = 
        f
        |> readLines
        |> Array.toList
        |> List.map parseInputLine
    let firstPass = instructions |> List.scan (fun a b -> a + b) 0
    let setRes = createSet firstPass Set.empty
    match setRes with
    | Left x -> x
    | Right s -> 
        let finalSum = firstPass |> List.rev |> List.take 1 |> List.exactlyOne
        if finalSum = 0 then 0 
        else 
            let withoutFinal = firstPass |> List.take (List.length firstPass - 1)
            findResult withoutFinal finalSum