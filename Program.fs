let dayData x = System.IO.File.ReadAllText (sprintf "..\..\Data\%i.txt" x)
let data = "" :: ([1..3] |> List.map dayData)

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

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
