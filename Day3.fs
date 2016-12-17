/// The design document gives the side lengths of each triangle it describes, but... 
/// 5 10 25? Some of these aren't triangles. You can't help but mark the impossible ones.
/// 
/// In a valid triangle, the sum of any two sides must be larger than the remaining side. 
/// For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.
/// 
/// In your puzzle input, how many of the listed triangles are possible?
module Day3

open Utils

let private parseTriangle (s: string) =
  match splitStringBy s " " with
  | [Int a; Int b; Int c] -> Some [a; b; c]
  | _ -> None

let private isValid (items: int list) =
  let sum = List.sum items 
  items |> List.forall (fun i -> sum - i > i)

let validTriangleCount (lines: string) =
  splitStringBy lines "\n" 
  |> List.choose parseTriangle
  |> List.filter isValid
  |> List.length

/// Now that you've helpfully marked up their design documents, it occurs to you that 
/// triangles are specified in groups of three vertically. Each set of three numbers 
/// in a column specifies a triangle. Rows are unrelated.

/// For example, given the following specification, numbers with the same hundreds 
/// digit would be part of the same triangle:
/// 101 301 501
/// 102 302 502
/// 103 303 503
/// 201 401 601
/// 202 402 602
/// 203 403 603
/// In your puzzle input, and instead reading by columns, how many of the listed 
/// triangles are possible?

let validTriangleByColumnsCount (lines: string) =
  let rows = 
    splitStringBy lines "\n"
    |> List.choose parseTriangle

  [0..2]
  |> List.map (fun i -> rows |> List.map (fun x -> x.[i]))
  |> List.concat
  |> List.chunkBySize 3
  |> List.filter isValid
  |> List.length