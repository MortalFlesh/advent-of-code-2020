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
            let expanses = expanses |> List.map int

            expanses
            |> List.map int
            |> List.tryPick (fun a ->
                expanses
                |> List.tryFind (fun b -> a + b = 2020)
                |> Option.map (fun b -> a, b)
            )
            |> Option.map (fun (a, b) -> a * b)

        /// Find three numbers in your expense report that meet the same criteria
        let tryFind3MatchingNumbers (expanses: string list) =
            let expanses = expanses |> List.map int

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

    [<RequireQualifiedAccess>]
    module Day2 =
        /// Each line gives the password policy and then the password.
        /// The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid.
        /// For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
        let countValidPasswords passwords =
            passwords
            |> List.filter (function
                | Regex @"^(\d+)-(\d+) (\w): (.+)$" [ min; max; letter; password ] ->
                    let count =
                        password
                        |> Seq.filter ((=) letter.[0])
                        |> Seq.length

                    count >= (int min) && count <= (int max)
                | _ -> false
            )
            |> List.length

    let args = [
        Argument.required "day" "A number of a day you are running"
        Argument.required "input" "Input data file path"
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

        let! file =
            input
            |> Input.getArgumentValueAsString "input"
            |> Result.ofOption "Missing input file"

        let inputLines =
            file
            |> FileSystem.readLines

        let secondPuzzle =
            match input with
            | Input.HasOption "second-puzzle" _ -> true
            | _ -> false

        let handleResult dayResult = result {
            match expected with
            | Some expected ->
                do! dayResult |> Assert.eq (int expected)
                return "Done"
            | _ ->
                return sprintf "Result value is %A" dayResult
        }

        match day with
        | 1 ->
            let! day1result =
                if secondPuzzle
                then inputLines |> Day1.tryFind3MatchingNumbers
                else inputLines |> Day1.tryFind2MatchingNumbers
                |> Result.ofOption "There are no numbers in the input which matches a criteria."

            return! handleResult day1result
        | 2 ->
            let day2result =
                if secondPuzzle
                then failwith "not implemented yet"
                else inputLines |> Day2.countValidPasswords

            return! handleResult day2result
        | day ->
            return! Error <| sprintf "Day %A is not ready yet." day
    })
