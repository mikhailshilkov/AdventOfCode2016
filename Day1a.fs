/// The Document indicates that you should start at the given coordinates 
/// (where you just landed) and face North. Then, follow the provided sequence: 
/// either turn left (L) or right (R) 90 degrees, then walk forward the given 
/// number of blocks, ending at a new intersection.
///
/// There's no time to follow such ridiculous instructions on foot, though, so 
/// you take a moment and work out the destination. Given that you can only walk 
/// on the street grid of the city, how far is the shortest path to the destination?
///
/// For example:
///
/// Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
/// R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
/// R5, L5, R5, R3 leaves you 12 blocks away.
///
/// How many blocks away is Easter Bunny HQ?
module Day1a

open Utils

type private Direction = | West | North | East | South
type private Turn = | Left | Right

type private Command = {
  Turn: Turn
  Steps: int
}

type private Position = {
  FacingTowards: Direction
  X: int
  Y: int
}

let private parseCommand (c: string) =
  let turn = match c.[0] with | 'L' -> Some Left | 'R' -> Some Right | _ -> None
  match turn, c.Substring(1) with
  | Some t, Int w -> Some { Turn = t; Steps = w}
  | _ -> None

let private walk position steps = 
  match position.FacingTowards with
  | West -> { position with X = position.X - steps }
  | East -> { position with X = position.X + steps }
  | North -> { position with Y = position.Y - steps }
  | South -> { position with Y = position.Y + steps }
    
let private makeMove position move =
  let newFacindTowards = 
    match position.FacingTowards, move.Turn with
    | North, Left -> West
    | North, Right -> East
    | West, Left -> South
    | West, Right -> North
    | South, Left -> East
    | South, Right -> West
    | East, Left -> North
    | East, Right -> South
  walk { position with FacingTowards = newFacindTowards } move.Steps

let run (path: string) : int =
  path.Split(',') 
  |> List.ofArray 
  |> List.map (fun x -> x.Trim()) 
  |> List.filter (fun x -> x <> "")
  |> List.choose parseCommand
  |> List.fold makeMove { FacingTowards = North; X = 0; Y = 0 }
  |> (fun finish -> finish.X + finish.Y)