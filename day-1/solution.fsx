open System
open System.IO

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

let runPartTwo f =
    f
    |> readLines
    |> Array.toList
    |> List.map parseInputLine
    |> List.scan (fun a x -> a + x) 0
    // |> List.fold (fun (xs,b,found) x -> 
    //     if (found) then (xs,b,found)
    //     else
    //         let exists = List.exists (fun i -> i = x) xs
    //         if (exists) then (xs, x, true)
    //         else (x::xs, b, found)) ([], 0, false)
    // |> extractResult