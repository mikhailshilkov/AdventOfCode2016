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
module Day1

open Utils

type private Direction = | West | North | East | South
type private Turn = | Left | Right

type private Command = {
  Turn: Turn
  Steps: int
}

type private Position = {
  X: int
  Y: int
}

type private DirectedPosition = {
  FacingTowards: Direction
  Position: Position
}

let private parseCommand (c: string) =
  let turn = match c.[0] with | 'L' -> Some Left | 'R' -> Some Right | _ -> None
  match turn, c.Substring(1) with
  | Some t, Int w -> Some { Turn = t; Steps = w}
  | _ -> None

let private parseCommands (path: string) = 
  path.Split(',') 
  |> List.ofArray 
  |> List.map (fun x -> x.Trim()) 
  |> List.filter (fun x -> x <> "")
  |> List.choose parseCommand

let private takeTurn facing turn =
  match facing, turn with
  | North, Left -> West
  | North, Right -> East
  | West, Left -> South
  | West, Right -> North
  | South, Left -> East
  | South, Right -> West
  | East, Left -> North
  | East, Right -> South

let private walk steps direction position = 
  match direction with
  | West -> { position with X = position.X - steps }
  | East -> { position with X = position.X + steps }
  | North -> { position with Y = position.Y - steps }
  | South -> { position with Y = position.Y + steps }
    
let private makeMove (position: DirectedPosition) move =
  let turned = takeTurn position.FacingTowards move.Turn
  { FacingTowards = turned; Position = walk move.Steps turned position.Position }

let distanceToFinish (path: string) : int =
  parseCommands path
  |> List.fold makeMove { FacingTowards = North; Position = { X = 0; Y = 0 } }
  |> (fun finish -> finish.Position.X + finish.Position.Y)

/// Then, you notice the instructions continue on the back of the Recruiting Document. 
/// Easter Bunny HQ is actually at the first location you visit twice.
///
/// For example, if your instructions are R8, R4, R4, R8, the first location you visit 
/// twice is 4 blocks away, due East.
///
/// How many blocks away is the first location you visit twice?

type private DirectionCommand = {
  Direction: Direction
  Steps: int
}

let private toDirectional previous command =
  let newFacing = takeTurn previous.Direction command.Turn
  { Direction = newFacing; Steps = command.Steps }

let private toDeltas command =
  [1..command.Steps]
  |> List.map (fun _ -> walk 1 command.Direction)

let distanceToFirstDuplicateLocation (path: string) : int =
  let moves = 
    parseCommands path
    |> List.scan toDirectional { Direction = North; Steps = 0 }
    |> List.collect toDeltas

  let allPositions = moves |> List.scan (|>) { X = 0; Y = 0 }

  allPositions 
  |> Seq.countBy id 
  |> Seq.filter (fun (_, c) -> c >= 2) 
  |> Seq.map fst 
  |> Seq.head
  |> (fun finish -> finish.X + finish.Y)
