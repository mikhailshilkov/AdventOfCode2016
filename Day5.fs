/// The eight-character password for the door is generated one character at a 
/// time by finding the MD5 hash of some Door ID (your puzzle input) and an 
/// increasing integer index (starting with 0).
/// 
/// A hash indicates the next character in the password if its hexadecimal 
/// representation starts with five zeroes. If it does, the sixth character 
/// in the hash is the next character of the password.
/// 
/// For example, if the Door ID is abc:
/// 
/// The first index which produces a hash that starts with five zeroes is 
/// 3231929, which we find by hashing abc3231929; the sixth character of the 
/// hash, and thus the first character of the password, is 1.
/// 5017308 produces the next interesting hash, which starts with 000008f82..., 
/// so the second character of the password is 8.
/// The third time a hash starts with five zeroes is for abc5278568, discovering 
/// the character f.
/// In this example, after continuing this search a total of eight times, the 
/// password is 18f47a30.
/// 
/// Given the actual Door ID, what is the password?
module Day5

open System.Security.Cryptography
open System.Text
open Utils

let private md5hasher = MD5.Create()

let private md5 (input: string) : string =
  input
  |> System.Text.Encoding.ASCII.GetBytes
  |> md5hasher.ComputeHash
  |> Array.take 4
  |> Array.map (fun c -> c.ToString("X2"))
  |> String.concat ""

let getPassword1 door =
  Seq.initInfinite id
  |> Seq.map (fun i -> door + i.ToString() |> md5)
  |> Seq.filter (fun hash -> hash.StartsWith "00000")
  |> Seq.map (fun hash -> hash.[5])
  |> Seq.take 8
  |> toString

/// Instead of simply filling in the password from left to right, the hash 
/// now also indicates the position within the password to fill. You still look 
/// for hashes that begin with five zeroes; however, now, the sixth character 
/// represents the position (0-7), and the seventh character is the character 
/// to put in that position.
/// 
/// A hash result of 000001f means that f is the second character in the password. 
/// Use only the first result for each position, and ignore invalid positions.
/// 
/// For example, if the Door ID is abc:
/// 
/// The first interesting hash is from abc3231929, which produces 0000015...; 
/// so, 5 goes in position 1: _5______.
/// In the previous method, 5017308 produced an interesting hash; however, 
/// it is ignored, because it specifies an invalid position (8).
/// The second interesting hash is at index 5357525, which produces 000004e...; 
/// so, e goes in position 4: _5__e___.
/// You almost choke on your popcorn as the final character falls into place, 
/// producing the password 05ace8e3.
/// 
/// Given the actual Door ID and this new method, what is the password?

let getPassword2 door =
  Seq.initInfinite id
  |> Seq.map (fun i -> door + i.ToString() |> md5)
  |> Seq.filter (fun hash -> hash.StartsWith "00000" && hash.[5] >= '0' && hash.[5] <= '7')
  |> Seq.map (fun hash -> hash.[5], hash.[6])
  |> Seq.distinctBy fst
  |> Seq.take 8
  |> Seq.sortBy fst
  |> Seq.map snd
  |> toString