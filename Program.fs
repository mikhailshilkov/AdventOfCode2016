let dayData x = System.IO.File.ReadAllText (sprintf "..\..\Data\%i.txt" x)
let data = "" :: ([1..4] |> List.map dayData)

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

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
