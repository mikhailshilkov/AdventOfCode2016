/// An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. 
/// An ABBA is any four-character sequence which consists of a pair of two different 
/// characters followed by the reverse of that pair, such as xyyx or abba. However, 
/// the IP also must not have an ABBA within any hypernet sequences, which are 
/// contained by square brackets.
/// 
/// For example:
/// 
/// abba[mnop]qrst supports TLS (abba outside square brackets).
/// abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though 
/// xyyx is outside square brackets).
/// aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters 
/// must be different).
/// ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though 
/// it's within a larger string).
/// How many IPs in your puzzle input support TLS?
module Day7
open Utils

let private containsPattern =
  Seq.windowed 4 >> Seq.exists (fun [|a; b; c; d|] -> a <> b && a = d && b = c)

let private partitionNets s =
  splitStringByMany ["["; "]"] s
  |> partitioni (fun (i, _) -> i % 2 = 0)

let private matchTLS s =
  let (supernets, hypernets) = partitionNets s
  supernets |> List.exists containsPattern 
  && hypernets |> List.exists containsPattern |> not

let private countIPs f input =
  input 
  |> splitStringBy "\n"
  |> List.filter f
  |> List.length

let countTLS = countIPs matchTLS

/// An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the 
/// supernet sequences (outside any square bracketed sections), and a corresponding 
/// Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An ABA is any 
/// three-character sequence which consists of the same character twice with a 
/// different character between them, such as xyx or aba. A corresponding BAB is 
/// the same characters but in reversed positions: yxy and bab, respectively.
/// 
/// For example:
/// 
/// aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab 
/// within square brackets).
/// xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
/// aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; 
/// the aaa sequence is not related, because the interior character must be different).
/// zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a 
/// corresponding bzb, even though zaz and zbz overlap).
/// How many IPs in your puzzle input support SSL?
type ABA = { A: char; B: char }
let reverse aba = { A = aba.B; B = aba.A }

let private findABAs =
  Seq.windowed 3
  >> Seq.filter (fun [|a; b; c|] -> a = c && a <> b)
  >> Seq.map (fun [|a; b; c|] -> { A = a; B = b })
  >> Set.ofSeq

let private matchSSL s =
  let (supernets, hypernets) = partitionNets s
  let abas = supernets |> List.map findABAs |> Set.unionMany
  let babs = hypernets |> List.map findABAs |> Set.unionMany |> Set.map reverse
  Set.intersect abas babs |> Set.isEmpty |> not

let countSSL = countIPs matchSSL