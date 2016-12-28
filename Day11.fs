/// Description - see http://adventofcode.com/2016/day/11
module Day11
open Utils

type PieceType = Microchip | Generator

type Piece = {
  Name: string
  Type: PieceType
}

let private isValidFloor pieces =
  let generators = 
    pieces 
    |> Set.filter (fun x -> x.Type = Generator) 
    |> Set.map (fun x -> x.Name)
  let unmatchedMicrochips =
    pieces 
    |> Set.filter (fun x -> x.Type = Microchip)
    |> Set.map (fun x -> x.Name)
    |> Set.difference <| generators
  Set.isEmpty generators || Set.isEmpty unmatchedMicrochips

let private possiblePicks pieces = 
  seq {
    for e1 in pieces do
      yield Set [e1]
      for e2 in pieces do
        if e1 > e2 then
          yield Set [e1; e2]
  }

type private Situation = { Floors: List<Set<Piece>>; ElevatorAt: int}
let private hash situation = 
  let nameMap =
    situation.Floors
    |> List.collect (fun f -> f |> Set.toList |> List.map (fun x -> x.Name))
    |> List.distinct
    |> List.mapi (fun i n -> n, i.ToString())
    |> Map.ofList
  let pattern = 
    situation.Floors
    |> List.mapi (fun i f -> sprintf "%i-%s" i (f |> Set.toList |> List.map (fun x -> nameMap.[x.Name] + if x.Type = Generator then "G" else "M") |> join ""))
    |> join ","
  sprintf "%i:%s" situation.ElevatorAt pattern

let private move floors moving fromFloor toFloor =
  let newFloors =
    floors
    |> List.mapi (fun i floor ->
      if i = fromFloor then floors.[i] |> Set.difference <| moving
      elif i = toFloor then floors.[i] |> Set.union moving
      else floors.[i]
    )
  { Floors = newFloors; ElevatorAt = toFloor }

let private possibleTransitions visited (floors: Situation) =
  seq {
    for toFloor in 0..3 do
      if abs(floors.ElevatorAt - toFloor) = 1 then
        for moving in possiblePicks floors.Floors.[floors.ElevatorAt] ->
          move floors.Floors moving floors.ElevatorAt toFloor
  }

let rec private tree steps initial visited =
  seq {
    for t in initial do 
      yield (steps, t)

    let transitions = 
      initial
      |> Seq.collect (possibleTransitions visited)
      |> Seq.filter (fun x -> List.forall isValidFloor x.Floors)
      |> Seq.map (fun x -> x, hash x)
      |> Seq.distinctBy snd
      |> Seq.filter (fun (_, h) -> not (Set.contains h visited))
      |> Seq.map fst
      |> List.ofSeq

    let newvisited = visited |> Set.union (Set.ofList transitions |> Set.map hash)

    yield! tree (steps + 1) transitions newvisited
  }

let countOfSteps floors =
  let amount = floors |> List.map (fun x -> Set.count x) |> List.sum
  let initial = { Floors = floors; ElevatorAt = 0}

  tree 0 [initial] (Set [hash initial])
  |> Seq.skipWhile (fun (c, fs) -> Set.count fs.Floors.[3] < amount)
  |> Seq.head
  |> fst

let floors1 = [
  Set [ { Name = "Pr"; Type = Generator }; { Name = "Pr"; Type = Microchip } ]
  Set [ { Name = "Co"; Type = Generator }; { Name = "Cu"; Type = Generator }; { Name = "Ru"; Type = Generator }; { Name = "Pl"; Type = Generator };  ]
  Set [ { Name = "Co"; Type = Microchip }; { Name = "Cu"; Type = Microchip }; { Name = "Ru"; Type = Microchip }; { Name = "Pl"; Type = Microchip };]
  Set [ ]
]

let floors2 = [
  Set [ { Name = "Pr"; Type = Generator }; { Name = "Pr"; Type = Microchip }; { Name = "El"; Type = Generator }; { Name = "El"; Type = Microchip }; { Name = "Di"; Type = Generator }; { Name = "Di"; Type = Microchip } ]
  Set [ { Name = "Co"; Type = Generator }; { Name = "Cu"; Type = Generator }; { Name = "Ru"; Type = Generator }; { Name = "Pl"; Type = Generator };  ]
  Set [ { Name = "Co"; Type = Microchip }; { Name = "Cu"; Type = Microchip }; { Name = "Ru"; Type = Microchip }; { Name = "Pl"; Type = Microchip };]
  Set [ ]
]
