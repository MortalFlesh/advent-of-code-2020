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
        let private tryFindMatchingNumber expanses =
            expanses
            |> List.tryPick (fun a ->
                expanses
                |> List.tryFind (fun b -> a + b = 2020)
                |> Option.map (fun b -> a, b)
            )
            |> Option.map (fun (a, b) -> a * b)

        let execute expanses =
            expanses
            |> List.map int
            |> tryFindMatchingNumber

    let args = [
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

        let! expanses =
            input
            |> Input.getArgumentValueAsString "expanses"
            |> Result.ofOption "Missing expanses file"

        let! day1Result =
            expanses
            |> FileSystem.readLines
            |> Day1.execute
            |> Result.ofOption "There are no numbers in the input which matches a criteria."

        match expected with
        | Some expected ->
            return! day1Result |> Assert.eq (int expected) <!> (fun _ -> "Done")
        | _ ->
            return sprintf "Result value is %A" day1Result
    })
