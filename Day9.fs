/// The format compresses a sequence of characters. Whitespace is ignored. 
/// To indicate that some sequence should be repeated, a marker is added 
/// to the file, like (10x2). To decompress this marker, take the subsequent 
/// 10 characters and repeat them 2 times. Then, continue reading the file 
/// after the repeated data. The marker itself is not included in the 
/// decompressed output.
/// 
/// If parentheses or other characters appear within the data referenced by 
/// a marker, that's okay - treat it like normal data, not a marker, and 
/// then resume looking for markers after the decompressed section.
/// 
/// For example:
/// 
/// ADVENT contains no markers and decompresses to itself with no changes, 
/// resulting in a decompressed length of 6.
/// A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for 
/// a decompressed length of 7.
/// (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
/// A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for 
/// a decompressed length of 11.
/// (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, 
/// but because it's within a data section of another marker, it is not 
/// treated any differently from the A that comes after it. It has a 
/// decompressed length of 6.
/// X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length 
/// of 18), because the decompressed data from the (8x2) marker (the 
/// (3x3)ABC) is skipped and not processed further.
/// What is the decompressed length of the file (your puzzle input)? Don't 
/// count whitespace.
module Day9
open System
open Utils

let private (|CompressedBlock|_|) chars =
  match chars with
  | '(' :: rem ->
    let pattern = List.takeWhile ((<>) ')') rem
    match pattern |> toString with
    | TwoIntegers "" "x" (x, y) -> 
      let further = List.skip (pattern.Length + 1) rem
      Some (x, y, further)
    | _ -> None
  | _ -> None

let rec private decompressChars acc = function
  | [] -> acc
  | t :: rem when Char.IsWhiteSpace t -> decompressChars acc rem
  | CompressedBlock (len, times, rem) ->
    let sublen = times * len |> uint64
    decompressChars (acc + sublen) (List.skip len rem)
  | t :: rem -> decompressChars (acc + 1UL) rem

let private decompress f (input: string) =
  input.ToCharArray()
  |> List.ofArray
  |> f 0UL

let decompress1 = decompress decompressChars

/// In version two, the only difference is that markers within 
/// decompressed data are decompressed. This, the documentation explains, 
/// provides much more substantial compression capabilities, allowing 
/// many-gigabyte files to be stored in only a few kilobytes.

let rec private decompressChars2 acc = function
  | [] -> acc
  | t :: rem when Char.IsWhiteSpace t -> decompressChars2 acc rem
  | CompressedBlock (len, times, rem) ->
    let sublen = decompressChars2 0UL (List.take len rem)
    let repetitionLength = sublen * uint64 times
    decompressChars2 (acc + repetitionLength) (List.skip len rem)
  | t :: rem -> decompressChars2 (acc + 1UL) rem

let decompress2 = decompress decompressChars2
