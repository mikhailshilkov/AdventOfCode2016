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

let (|TwoIntegers|_|) (prefix:string) (mid:string) (s:string) =
  if s.StartsWith(prefix) then
    let parts = splitStringBy mid (s.Substring(prefix.Length))
    match parts with
    | [Int i1; Int i2] -> Some (i1, i2)
    | _ -> None
  else None

let (|ThreeIntegers|_|) (prefix:string) (mid1:string) (mid2:string) (s:string) =
  if s.StartsWith(prefix) then
    let parts = splitStringBy mid1 (s.Substring(prefix.Length))
    match parts with
    | [Int i1; TwoIntegers "" mid2 (i2, i3)] -> Some (i1, i2, i3)
    | _ -> None
  else None

let toString (chars: char seq) = new string(chars |> Array.ofSeq)

let partitioni f list =
  list
  |> List.mapi (fun i x -> i, x)
  |> List.partition f
  |> fun (l1, l2) -> l1 |> List.map snd, l2 |> List.map snd

let foldWhile f initial input =
  // Generate sequence of all intermediate states
  input |> Seq.scan (fun stateOpt inp -> 
       // If the current state is not 'None', then calculate a new one
       // if 'f' returns 'None' then the overall result will be 'None'
       stateOpt |> Option.bind (fun state -> f state inp)) (Some initial)
  // Take only 'Some' states and get the last one
  |> Seq.takeWhile Option.isSome
  |> Seq.last |> Option.get

let findFold finder f initial input =
  Seq.initInfinite id
  |> foldWhile (fun (rem, state) item ->
    match rem with
    | [] -> None
    | _ -> 
      let next = rem |> List.find (finder state)
      Some (rem |> List.except [next], f state next)
    ) (input, initial)
  |> snd
  