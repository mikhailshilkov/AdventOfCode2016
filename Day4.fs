/// Each room consists of an encrypted name (lowercase letters separated by dashes) 
/// followed by a dash, a sector ID, and a checksum in square brackets.
/// 
/// A room is real (not a decoy) if the checksum is the five most common letters in 
/// the encrypted name, in order, with ties broken by alphabetization. For example:
/// aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are 
/// a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
/// a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all 
/// tied (1 of each), the first five are listed alphabetically.
/// not-a-real-room-404[oarel] is a real room.
/// totally-real-room-200[decoy] is not.
/// Of the real rooms from the list above, the sum of their sector IDs is 1514.
/// 
/// What is the sum of the sector IDs of the real rooms?
module Day4

open Utils

type private Room = {
  Name: string
  SectorId: int
  Checksum: string
}

let private parseRoom (line: string) =
  let lastDashIndex = line.LastIndexOf '-'
  let remainderParts = splitStringByMany ["["; "]"] (line.Substring(lastDashIndex + 1))
  { Name = line.Substring(0, lastDashIndex).Replace("-", "")
    SectorId = remainderParts.[0] |> parseInt |> Option.get
    Checksum = remainderParts.[1] }

let private checksum (name: string) =
  name.ToCharArray()
  |> Array.countBy id
  |> Array.sortBy (fun (c, n) -> -n, c)
  |> Array.map fst
  |> Array.take 5
  |> (fun cs -> new string(cs))

let private isValidChecksum room = checksum room.Name = room.Checksum

let sumOfSectorIds input =
  input
  |> splitStringBy "\n"
  |> List.map parseRoom
  |> List.filter isValidChecksum
  |> List.sumBy (fun r -> r.SectorId)

/// The room names are encrypted by a state-of-the-art shift cipher, which 
/// is nearly unbreakable without the right software. However, the information 
/// kiosk designers at Easter Bunny HQ were not expecting to deal with a master 
/// cryptographer like yourself.
/// 
/// To decrypt a room name, rotate each letter forward through the alphabet a 
/// number of times equal to the room's sector ID. A becomes B, B becomes C, Z 
/// becomes A, and so on. Dashes become spaces.
/// 
/// For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
/// 
/// What is the sector ID of the room where North Pole objects are stored?
let private decode room =
  let abase = 'a' |> int
  room.Name.ToCharArray()
  |> Array.map (fun c -> ((c |> int) + room.SectorId - abase) % 26 + abase |> char)
  |> fun cs -> new string(cs)

let lookupSectorID input name =
  input
  |> splitStringBy "\n"
  |> List.map parseRoom
  |> List.filter isValidChecksum
  |> List.filter (fun r -> (decode r).StartsWith name)
  |> List.map (fun r -> r.SectorId)
  |> List.tryHead