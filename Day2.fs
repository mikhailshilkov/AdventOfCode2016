/// "In order to improve security," the document you find says, "bathroom codes will no longer 
/// be written down. Instead, please memorize and follow the procedure below to access the bathrooms."
///
/// The document goes on to explain that each button to be pressed can be found by starting on the 
/// previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves 
/// left, and R moves right. Each line of instructions corresponds to one button, starting at the 
/// previous button (or, for the first line, the "5" button); press whatever button you're on at the 
/// end of each line. If a move doesn't lead to a button, ignore it.
///
/// You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. 
/// You picture a keypad like this:
///
/// 1 2 3
/// 4 5 6
/// 7 8 9
/// Suppose your instructions are:
///
/// ULL
/// RRDDD
/// LURDL
/// UUUUD
/// You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), 
/// so the first button is 1.
/// Starting from the previous button ("1"), you move right twice (to "3") and then down three times 
/// (stopping at "9" after two moves and ignoring the third), ending up with 9.
/// Continuing from "9", you move left, up, right, down, and left, ending with 8.
/// Finally, you move up four times (stopping at "2"), then down once, ending with 5.
/// So, in this example, the bathroom code is 1985.
/// 
/// Your puzzle input is the instructions from the document you found at the front desk. What is the 
/// bathroom code?

module Day2

type private Key = | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
type private Command = | Up | Down | Left | Right

let private move p c =
  match p, c with
  | One, Right -> Two
  | One, Down -> Four
  | Two, Left -> One
  | Two, Right -> Three
  | Two, Down -> Five
  | Three, Left -> Two
  | Three, Down -> Six
  | Four, Up -> One
  | Four, Right -> Five
  | Four, Down -> Seven
  | Five, Up -> Two
  | Five, Left -> Four
  | Five, Right -> Six
  | Five, Down -> Eight
  | Six, Left -> Five
  | Six, Up -> Three
  | Six, Down -> Nine
  | Seven, Up -> Four
  | Seven, Right -> Eight
  | Eight, Up -> Five
  | Eight, Left -> Seven
  | Eight, Right -> Nine
  | Nine, Up -> Six
  | Nine, Left -> Eight
  | _ -> p

let private parseCommand = function
  | 'U' -> Some Up
  | 'D' -> Some Down
  | 'L' -> Some Left
  | 'R' -> Some Right
  | _ -> None

let private printKey = function
  | One -> '1'
  | Two -> '2'
  | Three -> '3'
  | Four -> '4'
  | Five -> '5'
  | Six -> '6'
  | Seven -> '7'
  | Eight -> '8'
  | Nine -> '9'

let private parseCommands (s: string) =
  s.ToCharArray() |> List.ofArray |> List.choose parseCommand

let private navigate m p moves = moves |> List.fold m p

let private getCode move print start (moves: string): string =
  moves.Split('\n') 
  |> List.ofArray 
  |> List.map parseCommands
  |> List.scan (navigate move) start
  |> List.tail
  |> List.map print
  |> (fun cs -> new string(Array.ofList cs))

let getCode1to9 = getCode move printKey Five

/// You finally arrive at the bathroom (it's a several minute walk from the lobby so 
/// visitors can behold the many fancy conference rooms and water coolers on this floor) 
/// and go to punch in the code. Much to your bladder's dismay, the keypad is not at all 
/// like you imagined it. Instead, you are confronted with the result of hundreds of man-hours 
/// of bathroom-keypad-design meetings:
/// 
///     1
///   2 3 4
/// 5 6 7 8 9
///   A B C
///     D
/// You still start at "5" and stop when you're at an edge, but given the same 
/// instructions as above, the outcome is very different:

/// You start at "5" and don't move at all (up and left are both edges), ending at 5.
/// Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), 
/// ending at D.
/// Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
/// Finally, after five more moves, you end at 3.
/// So, given the actual keypad layout, the code would be 5DB3.
/// 
/// Using the same instructions in your puzzle input, what is the correct bathroom code?

type ExtendedKey = | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | A | B | C | D

let private moveExtended p c =
  match p, c with
  | One, Down -> Three
  | Two, Right -> Three
  | Two, Down -> Six
  | Three, Left -> Two
  | Three, Up -> One
  | Three, Right -> Four
  | Three, Down -> Seven
  | Four, Left -> Three
  | Four, Down -> Eight
  | Five, Right -> Six
  | Six, Up -> Two
  | Six, Left -> Five
  | Six, Right -> Seven
  | Six, Down -> A
  | Seven, Up -> Three
  | Seven, Left -> Six
  | Seven, Right -> Eight
  | Seven, Down -> B
  | Eight, Up -> Four
  | Eight, Left -> Seven
  | Eight, Right -> Nine
  | Eight, Down -> C
  | Nine, Left -> Eight
  | A, Up -> Six
  | A, Right -> B
  | B, Up -> Seven
  | B, Left -> A
  | B, Right -> C
  | B, Down -> D
  | C, Up -> Eight
  | C, Left -> B
  | D, Up -> B
  | _ -> p

let private printExtendedKey = function
  | One -> '1'
  | Two -> '2'
  | Three -> '3'
  | Four -> '4'
  | Five -> '5'
  | Six -> '6'
  | Seven -> '7'
  | Eight -> '8'
  | Nine -> '9'
  | A -> 'A'
  | B -> 'B'
  | C -> 'C'
  | D -> 'D'

let getCode1toD = getCode moveExtended printExtendedKey Five