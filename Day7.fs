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

let rec private containsPattern (s: string) =
  if s.Length >= 4 then
    s.[0] = s.[3] && s.[1] = s.[2] && s.[0] <> s.[1] || containsPattern (s.Substring 1)
  else false

let private matchIP s =
  let parts = splitStringByMany ["["; "]"] s
  parts
  |> List.mapi (fun i x -> i, x)
  |> List.partition (fun (i, x) -> i % 2 = 0)
  |> fun (ips, hypernets) -> 
    ips |> List.map snd |> List.exists containsPattern 
    && hypernets |> List.map snd |> List.exists containsPattern |> not

let countIPs input =
  input 
  |> splitStringBy "\n"
  |> List.filter matchIP
  |> List.length

