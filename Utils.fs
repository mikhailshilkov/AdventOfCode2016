﻿module Utils

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

let toString (chars: char seq) = new string(chars |> Array.ofSeq)

let partitioni f list =
  list
  |> List.mapi (fun i x -> i, x)
  |> List.partition f
  |> fun (l1, l2) -> l1 |> List.map snd, l2 |> List.map snd