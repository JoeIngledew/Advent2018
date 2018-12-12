open System
open System.IO

type Item =
| Two of char
| Three of char
| NoMatch

let isThreeIsTwo (id: string) =
    let d = 
        id.ToCharArray() 
        |> Array.toList
        |> Seq.groupBy (fun c -> c)
        |> Map.ofSeq
        |> Map.map (fun k v -> Seq.length v)
    let threes = d |> Map.tryPick (fun k v -> if v = 3 then Some(k) else None)
    let twos =  d |> Map.tryPick (fun k v -> if v = 2 then Some(k) else None)
    threes, twos

let multiplyTuple (a,b) = a * b

let getChecksum f =
    File.ReadAllLines(f)
    |> Array.toList
    |> List.map isThreeIsTwo
    |> List.fold (fun prev next -> 
        let threes,twos = prev
        match next with
        | Some _, Some _ -> (threes+1), (twos+1)
        | Some _, None -> (threes+1),twos
        | None, Some _ -> threes, (twos+1)
        | _ -> threes,twos) (0, 0)
    |> multiplyTuple    

let getStringDiff (s1: string) (s2: string) = 
    let xs = s1.ToCharArray() |> Array.toList
    let ys = s2.ToCharArray() |> Array.toList
    let xys = List.zip xs ys
    (xys |> List.mapi (fun ix (a,b) -> if a = b then -1 else ix)),s1

let getSharedChars ((diffs: int list),(s: string)) =
    s.ToCharArray()
    |> Array.toList
    |> List.zip diffs
    |> List.filter (fun (a,b) -> a = -1)

let getSimilar f =
    let lines = 
        File.ReadAllLines(f)
        |> Array.toList
    lines
    |> List.map (fun l -> lines |> List.filter (fun l2 -> l2 <> l) |> List.map (fun l3 -> getStringDiff l l3))
    |> List.filter (fun l -> l |> List.exists (fun (diffs,_) -> diffs |> List.filter (fun d -> d <> -1) |> List.length = 1))
    |> List.concat
    |> List.find (fun (diffs,_) ->
        let numdiffs = diffs |> List.filter (fun x -> x <> -1) |> List.length
        numdiffs = 1)
    |> getSharedChars
    |> List.map snd
    |> List.fold (fun s n -> sprintf "%s%c" s n) ""