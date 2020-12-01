namespace MF.AdventOfCode.Command

[<RequireQualifiedAccess>]
module AdventOfCode =
    open System.IO
    open MF.ConsoleApplication
    open MF.AdventOfCode.Console
    open MF.ErrorHandling
    open MF.ErrorHandling.Result.Operators
    open MF.Utils

    [<RequireQualifiedAccess>]
    module private Day1 =

        /// Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
        let tryFind2MatchingNumbers expanses =
            expanses
            |> List.tryPick (fun a ->
                expanses
                |> List.tryFind (fun b -> a + b = 2020)
                |> Option.map (fun b -> a, b)
            )
            |> Option.map (fun (a, b) -> a * b)

        /// Find three numbers in your expense report that meet the same criteria
        let tryFind3MatchingNumbers expanses =
            expanses
            |> List.tryPick (fun a ->
                expanses
                |> List.tryPick (fun b ->
                    expanses
                    |> List.tryFind (fun c -> a + b + c = 2020)
                    |> Option.map (fun c -> a, b, c)
                )
            )
            |> Option.map (fun (a, b, c) -> a * b * c)

    let args = [
        Argument.required "day" "A number of a day you are running"
        Argument.required "expanses" "Expanse report"
        Argument.optional "expectedResult" "Expected result" None
    ]

    let execute: ExecuteCommand = fun (input, output) -> ExitCode.ofResult output (result {
        output.Title "Advent of Code 2020"

        let expected =
            input
            |> Input.getArgumentValueAsString "expectedResult"
            |> Option.map (fun expected ->
                if expected |> File.Exists
                    then expected |> FileSystem.readContent |> String.trim ' '
                    else expected
            )

        let day = input |> Input.getArgumentValueAsInt "day" |> Option.defaultValue 1

        match day with
        | 1 ->
            let! expanses =
                input
                |> Input.getArgumentValueAsString "expanses"
                |> Result.ofOption "Missing expanses file"

            let expanses =
                expanses
                |> FileSystem.readLines
                |> List.map int

            let day1result =
                match input with
                | Input.HasOption "second-puzzle" _ -> expanses |> Day1.tryFind3MatchingNumbers
                | _ -> expanses |> Day1.tryFind2MatchingNumbers

            let! day1result =
                day1result
                |> Result.ofOption "There are no numbers in the input which matches a criteria."

            match expected with
            | Some expected ->
                do! day1result |> Assert.eq (int expected)

                return "Done"
            | _ ->
                return sprintf "Result value is %A" day1result

        | day ->
            return! Error <| sprintf "Day %A is not ready yet." day
    })
