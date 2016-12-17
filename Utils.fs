module Utils

let (|Int|_|) str =
  match System.Int32.TryParse(str) with
  | (true,int) -> Some(int)
  | _ -> None

let parseInt (s: string) = 
  match s with
  | Int i -> Some i
  | _ -> None

let splitStringBy (s: string) (separator: string) = 
  s.Split([|separator|], System.StringSplitOptions.RemoveEmptyEntries) 
  |> List.ofArray
  |> List.map (fun x -> x.Trim())  