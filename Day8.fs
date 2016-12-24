/// Unfortunately, the screen has been smashed. You figured out how it works. 
/// Now you just have to work out what the screen would have displayed.
/// 
/// The magnetic strip on the card you swiped encodes a series of instructions 
/// for the screen; these instructions are your puzzle input. The screen is 50 
/// pixels wide and 6 pixels tall, all of which start off, and is capable of 
/// three somewhat peculiar operations:
/// 
/// rect AxB turns on all of the pixels in a rectangle at the top-left of the 
/// screen which is A wide and B tall.
/// rotate row y=A by B shifts all of the pixels in row A (0 is the top row) 
/// right by B pixels. Pixels that would fall off the right end appear at the 
/// left end of the row.
/// rotate column x=A by B shifts all of the pixels in column A (0 is the left 
/// column) down by B pixels. Pixels that would fall off the bottom appear at 
/// the top of the column.
/// For example, here is a simple sequence on a smaller screen:
/// 
/// rect 3x2 creates a small rectangle in the top-left corner:
/// ###....
/// ###....
/// .......
/// rotate column x=1 by 1 rotates the second column down by one pixel:
/// #.#....
/// ###....
/// .#.....
/// rotate row y=0 by 4 rotates the top row right by four pixels:
/// ....#.#
/// ###....
/// .#.....
/// rotate column x=1 by 1 again rotates the second column down by one pixel, 
/// causing the bottom pixel to wrap back to the top:
/// .#..#.#
/// #.#....
/// .#.....
/// 
/// There seems to be an intermediate check of the voltage used by the display: 
/// after you swipe your card, if the screen did work, how many pixels should be lit?
module Day8
open Utils

type private Instruction =
  | Rect of x: int * y: int
  | RotateRow of index: int * by: int
  | RotateColumn of index: int * by: int

let private (|TwoIntegers|_|) (prefix:string) (mid:string) (s:string) =
  if s.StartsWith(prefix) then
    let parts = splitStringBy mid (s.Substring(prefix.Length))
    match parts with
    | [Int i1; Int i2] -> Some (i1, i2)
    | _ -> None
  else None

let private parseInstruction = function
  | TwoIntegers "rect " "x" v-> Rect v |> Some
  | TwoIntegers "rotate row y=" " by " v -> RotateRow v |> Some
  | TwoIntegers "rotate column x=" " by " v -> RotateColumn v |> Some
  | _ -> None

let private Width = 50
let private Height = 6
type private Screen = Set<int * int>

let private rect w h screen = 
  seq { for x in 0..w-1 do for y in 0..h-1 -> x, y }
  |> Set.ofSeq
  |> Set.union screen

let private rotateRow index by =
  Set.map (fun (x, y) -> if y = index then (x + by) % Width, y else x, y)

let private rotateColumn index by =
  Set.map (fun (x, y) -> if x = index then x, (y + by) % Height else x, y)

let private execute screen = function
  | Rect (x, y) -> rect x y screen
  | RotateRow (i, by) -> rotateRow i by screen
  | RotateColumn (i, by) -> rotateColumn i by screen

let private display input =
  input
  |> splitStringBy "\n"
  |> List.choose parseInstruction
  |> List.fold execute Set.empty

let countLitPixels = display >> Set.count

/// You notice that the screen is only capable of displaying capital 
/// letters; in the font it uses, each letter is 5 pixels wide and 6 tall.
/// 
/// After you swipe your card, what code is the screen trying to display?
let printCode input =
  let data = display input
  let printLine y =
    [0..Width-1] 
    |> List.map (fun x -> if Set.contains (x, y) data then 'X' else ' ')
    |> toString
  [0..Height-1]
  |> List.map printLine