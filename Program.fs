open System.IO

let dayData x = 
  let filename = sprintf "..\..\Data\%i.txt" x
  if File.Exists filename then File.ReadAllText filename else ""
let data = "" :: ([1..26] |> List.map dayData)

[<EntryPoint>]
let main argv = 
    let day1a = Day1.distanceToFinish data.[1]
    printfn "Day 1A: %i" day1a

    let day1b = Day1.distanceToFirstDuplicateLocation data.[1]
    printfn "Day 1B: %i" day1b

    let day2a = Day2.getCode1to9 data.[2]
    printfn "Day 2A: %s" day2a

    let day2b = Day2.getCode1toD data.[2]
    printfn "Day 2B: %s" day2b

    let day3a = Day3.validTriangleCount data.[3]
    printfn "Day 3A: %i" day3a
    let day3b = Day3.validTriangleByColumnsCount data.[3]
    printfn "Day 3B: %i" day3b

    let day4a = Day4.sumOfSectorIds data.[4]
    printfn "Day 4A: %i" day4a
    let day4b = Day4.lookupSectorID data.[4] "northpole"
    printfn "Day 4B: %O" day4b

    let day5a = "too slow to run every time..." //Day5.getPassword1 "ffykfhsq"
    printfn "Day 5A: %s" day5a
    let day5b = "too slow to run every time..." //Day5.getPassword2' "ffykfhsq"
    printfn "Day 5B: %s" day5b

    let day6a = Day6.readMostPopular data.[6]
    printfn "Day 6A: %s" day6a
    let day6b = Day6.readLeastPopular data.[6]
    printfn "Day 6B: %s" day6b

    let day7a = Day7.countTLS data.[7]
    printfn "Day 7A: %i" day7a
    let day7b = Day7.countSSL data.[7]
    printfn "Day 7B: %i" day7b

    let day8a = Day8.countLitPixels data.[8]
    printfn "Day 8A: %i" day8a
    let day8b = Day8.printCode data.[8] 
    printfn "Day 8B:"
    day8b |> List.iter (printfn "%s")

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
