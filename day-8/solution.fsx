type Processed =
| NodeChildDef
| NodeMetaDef
| Meta

type Header = {
    Childs: int
    Metas: int    
}

let getHeader xs =
    { Childs= xs.[0]; Metas = xs.[1] }

let process (s: string) =
    let numList = s.Split(' ') |> Array.toList
