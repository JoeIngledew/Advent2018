let isLetterPair (c1: char) (c2: char) =
    let b1,b2 = (byte c1),(byte c2)
    let xor = 0x20 |> byte
    (b1 = (b2 ^^^ xor)) || (b2 = (b1 ^^^ xor))

let toString (cs: char list) =
    cs |> List.fold (fun s c -> sprintf "%s%c" s c) ""

let scanAndRemove (s: string) =
    let cs = s.ToCharArray() |> Array.toList
    let rec loop (prev: char option) xs letters =
        match xs with
        | [] -> letters 
        | y::ys -> 
            match prev with
            | None -> loop (Some(y)) ys letters
            | Some l -> 
                if isLetterPair y l then
                    loop None ys letters
                else loop (Some(l)) ys (l::letters)
    loop None cs []            
    |> List.rev
    |> toString

let scanAndRemove2 (s: string) =
    let cs = s.ToCharArray() |> Array.toList
    let matchesNext i = 
        if (i+1) = (List.length cs) then false
        elif isLetterPair cs.[i] cs.[i+1] then true
        else false
    let rec loop charsToCheck notMatched =
        match charsToCheck with 
        | [] -> notMatched
        | x::xs -> 
            match xs with
            | y::ys -> 
                if isLetterPair x y then loop ys notMatched 
                else loop xs (x::notMatched)
            | _ -> loop xs (x::notMatched)   
    let rec outerLoop chars opTaken =
        if not opTaken then chars
        else 
            let next = (loop chars []) |> List.rev
            // printfn "Next: %A" next
            // printfn "Current: %A" chars
            outerLoop next ((next |> List.length) <> (chars |> List.length))
    outerLoop cs true


let f = "./day-5/notes-5.txt"

let run f = 
    System.IO.File.ReadAllText(f)
    |> scanAndRemove2
    |> List.length

let findShortestByRemoval (cs: char list) : char=
    let allPossibilities = cs |> List.distinct
    [ for p in allPossibilities do yield (cs |> List.filter (fun x -> x = p) |> List.length),p ]
    |> List.minBy fst
    |> snd

let readFile f = System.IO.File.ReadAllText(f)

let isSameLetter c1 c2 = 
    c1 = c2 || (isLetterPair c1 c2)

// part two unsolved
let partTwo (f: string) =
    let chars: char list = f.ToCharArray() |> Array.toList
    let possibleTrims = f.ToLower().ToCharArray() |> Array.toList
    [
        for p in possibleTrims do yield chars |> List.filter (fun x -> not (isSameLetter x p)) |> toString |> scanAndRemove2 |> List.length
    ]
    |> List.min

    // let shortestChar: char =
    //     chars
    //     |> List.map (fun (c: char) -> System.Char.ToLower(c))
    //     |> findShortestByRemoval
    // chars
    // |> List.filter (fun (x: char) -> (System.Char.ToLower(x)) <> shortestChar)
    // |> toString
    // |> scanAndRemove2
    // |> List.length    