module Utils

let (|Int|_|) str =
  match System.Int32.TryParse(str) with
  | (true,int) -> Some(int)
  | _ -> None

let parseInt (s: string) = 
  match s with
  | Int i -> Some i
  | _ -> None

let splitStringByMany (separators: string list) (s: string) = 
  s.Split(separators |> Array.ofList, System.StringSplitOptions.RemoveEmptyEntries) 
  |> List.ofArray
  |> List.map (fun x -> x.Trim())  

let splitStringBy (separator: string) = splitStringByMany [separator]