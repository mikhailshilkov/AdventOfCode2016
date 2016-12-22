/// In this model, the same message is sent repeatedly. You've recorded the 
/// repeating message signal (your puzzle input), but the data seems quite 
/// corrupted - almost too badly to recover. Almost.
/// 
/// All you need to do is figure out which character is most frequent for each 
/// position. For example, suppose you had recorded the following messages:
/// 
/// eedadn
/// drvtee
/// eandsr
/// raavrd
/// ...
/// enarar
/// The most common character in the first column is e; in the second, a; in 
/// the third, s, and so on. Combining these characters returns the error-corrected 
/// message, easter.
/// 
/// Given the recording in your puzzle input, what is the error-corrected version 
/// of the message being sent?
module Day6
open Utils

let private findByCriteria compare input =
  let lines = splitStringBy "\n" input
  let pickPopular i =
    lines 
    |> List.map (fun s -> s.[i]) 
    |> List.countBy id 
    |> List.sortWith (fun a b -> if compare (snd a) (snd b) then 1 else -1)
    |> List.head
    |> fst

  [0..lines.[0].Length - 1]
  |> List.map pickPopular
  |> toString

let readMostPopular = findByCriteria (<)
let readLeastPopular = findByCriteria (>)