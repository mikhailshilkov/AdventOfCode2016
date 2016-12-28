/// Upon closer examination, you notice that each bot only proceeds when it 
/// has two microchips, and once it does, it gives each one to a different bot 
/// or puts it in a marked "output" bin. Sometimes, bots take microchips from 
/// "input" bins, too.
/// 
/// Inspecting one of the microchips, it seems like they each contain a single 
/// number; the bots must use some logic to decide what to do with each chip. 
/// You access the local control computer and download the bots' instructions 
/// (your puzzle input).
/// 
/// Some of the instructions specify that a specific-valued microchip should 
/// be given to a specific bot; the rest of the instructions indicate what a 
/// given bot should do with its lower-value or higher-value chip.
/// 
/// For example, consider the following instructions:
/// 
/// value 5 goes to bot 2
/// bot 2 gives low to bot 1 and high to bot 0
/// value 3 goes to bot 1
/// bot 1 gives low to output 1 and high to bot 0
/// bot 0 gives low to output 2 and high to output 0
/// value 2 goes to bot 2
/// Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 
/// chip and a value-5 chip.
/// Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and 
/// its higher one (5) to bot 0.
/// Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and 
/// gives the value-3 chip to bot 0.
/// Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 
/// in output 0.
/// In the end, output bin 0 contains a value-5 microchip, output bin 1 contains 
/// a value-2 microchip, and output bin 2 contains a value-3 microchip. In 
/// this configuration, bot number 2 is responsible for comparing value-5 
/// microchips with value-2 microchips.

/// Based on your instructions, what is the number of the bot that is 
/// responsible for comparing value-61 microchips with value-17 microchips?
module Day10
open Utils

type private InstructionValueTo = { Value: int; Bot: int }
type private InstructionLowHigh = { Bot: int; LowTo: int; HighTo: int }
let private output i = 1000 + i

let private parseInstruction1 = function
  | TwoIntegers "value " " goes to bot " (v, b) -> { Value = v; Bot = b } |> Some
  | _ -> None

let private parseInstruction2 = function
  | ThreeIntegers "bot " " gives low to bot " " and high to bot " (b, l, h) -> 
    { Bot = b; LowTo = l; HighTo = h } |> Some
  | ThreeIntegers "bot " " gives low to output " " and high to bot " (b, l, h) -> 
    { Bot = b; LowTo = output l; HighTo = h } |> Some
  | ThreeIntegers "bot " " gives low to bot " " and high to output " (b, l, h) -> 
    { Bot = b; LowTo = l; HighTo = output h } |> Some
  | ThreeIntegers "bot " " gives low to output " " and high to output " (b, l, h) -> 
    { Bot = b; LowTo = output l; HighTo = output h } |> Some
  | _ -> None

let private parseInstructions f input =
  input
  |> splitStringBy "\n"
  |> List.choose f

let private mapReplace key f map =
  let existing = Map.find key map
  Map.add key (f existing) map

let private buildBotNetwork input =
  let valueToInstuctions = parseInstructions parseInstruction1 input
  let lowHighInstructions = parseInstructions parseInstruction2 input

  let emptyBots = 
    List.append
      (valueToInstuctions |> List.map (fun v -> v.Bot))
      (lowHighInstructions |> List.collect (fun v -> [v.Bot; v.LowTo; v.HighTo]))
    |> List.distinct
    |> List.map (fun i -> i, [])
    |> Map.ofList

  let valueToBots = 
    valueToInstuctions
    |> List.fold (fun bs v -> 
      let bot = Map.find v.Bot bs
      Map.add v.Bot (v.Value :: bot) bs
      ) emptyBots

  lowHighInstructions
  |> findFold 
    (fun bots item -> 
      let values = Map.find item.Bot bots
      List.length values = 2) 
    (fun bots item ->
      let bot = Map.find item.Bot bots
      let [v1; v2] = List.sort bot
      bots
      |> mapReplace item.LowTo (fun x -> v1 :: x) 
      |> mapReplace item.HighTo (fun x -> v2 :: x)) 
    valueToBots
  |> Map.toList

let findBot v1 v2 =  
  buildBotNetwork
  >> List.find (fun (_, x) -> List.sort x = List.sort [v1; v2])
  >> fst

/// What do you get if you multiply together the values of one chip in 
/// each of outputs 0, 1, and 2?

let productOfOutput123 input = 
  buildBotNetwork input
  |> List.filter (fun x -> List.contains (fst x) [output 0; output 1; output 2])
  |> List.collect snd
  |> List.fold (*) 1