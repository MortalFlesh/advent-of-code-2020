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

        /// Each policy actually describes two positions in the password,
        /// where 1 means the first character, 2 means the second character, and so on.
        /// (Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
        /// Exactly one of these positions must contain the given letter.
        /// Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
        let countValidPasswordBySecondPolicy passwords =
            passwords
            |> List.filter (function
                | Regex @"^(\d+)-(\d+) (\w): (.+)$" [ first; second; letter; password ] ->
                    maybe {
                        let first = (int first) - 1
                        let second = (int second) - 1
                        let! firstLetter = password |> Seq.tryItem first
                        let! secondLetter = password |> Seq.tryItem second
                        let letter = letter.[0]

                        return
                            firstLetter <> secondLetter
                            && (firstLetter = letter || secondLetter = letter)
                    }
                    |> Option.defaultValue false
                | _ -> false
            )
            |> List.length

    [<RequireQualifiedAccess>]
    module Day3 =
        /// You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).
        /// The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:
        /// From your starting position at the top-left, check the position that is right 3 and down 1.
        /// Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.
        let countTreesInPathByR3D1 input =
            let value position (line: string) =
                //printfn "Check: %s at [%03d] | -> %A (%s)" line position line.[position] (if line.[position] = '#' then "tree" else "space")
                if line.[position] = '#' then 1 else 0

            let rec count (position, acc) = function
                | [] -> acc
                | (line: string) :: rest ->
                    rest |> count (
                        (position + 3) % line.Length,
                        acc + (line |> value position)
                    )

            input |> count (0, 0)

        /// Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:
        /// Right 1, down 1.
        /// Right 3, down 1. (This is the slope you already checked.)
        /// Right 5, down 1.
        /// Right 7, down 1.
        /// Right 1, down 2.
        /// In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
        let countTreesInPath input =
            let countTreesInPathBy input =
                let lines = input |> List.mapi (fun i v -> (i, v))

                let value position (line: string) =
                    // printfn "Check: %s at [%03d] | -> %A (%s)" line position line.[position] (if line.[position] = '#' then "tree" else "space")
                    if line.[position] = '#' then 1 else 0

                let rec count (right, down) (position, acc) = function
                    | [] -> acc
                    | (lineNumber, line: string) :: rest ->
                        rest |> count (right, down) (
                            (if lineNumber % down = 0
                                then (position + right) % line.Length
                                else position),

                            (if lineNumber % down = 0
                                then acc + (line |> value position)
                                else acc)
                        )

                fun (right, down) -> lines |> count (right, down) (0, 0)

            [
                1, 1
                3, 1
                5, 1
                7, 1
                1, 2
            ]
            |> List.fold (fun (acc: int64) (right, down) ->
                let c = countTreesInPathBy input (right, down)
                // printfn " - For %d, %d -> %d trees" right down c
                acc * (int64 c)
            ) (int64 1)

    [<RequireQualifiedAccess>]
    module Day4 =
        type private Passport = {
            /// byr
            BirthYear: string option
            /// iyr
            IssueYear: string option
            /// eyr
            ExpirationYear: string option
            /// hgt
            Height: string option
            /// hcl
            HairColor: string option
            /// ecl
            EyeColor: string option
            /// pid
            PassportID: string option
            /// cid
            CountryID: string option
        }

        let private isValid: Passport -> bool = function
            | { BirthYear = Some byr; IssueYear = Some iyr; ExpirationYear = Some eyr; Height = Some hgt; HairColor = Some hcl; EyeColor = Some ecl; PassportID = Some pid; CountryID = _ } -> true
            | _ -> false

        let private empty = {
            BirthYear = None
            IssueYear = None
            ExpirationYear = None
            Height = None
            HairColor = None
            EyeColor = None
            PassportID = None
            CountryID = None
        }

        let private parse = function
            | null | "" -> failwith "[Logic] Empty line can not be parsed."
            | line ->
                let rec parsePass passport = function
                    | [] -> passport
                    | ("byr", value) :: values -> values |> parsePass { passport with BirthYear = Some value }
                    | ("iyr", value) :: values -> values |> parsePass { passport with IssueYear = Some value }
                    | ("eyr", value) :: values -> values |> parsePass { passport with ExpirationYear = Some value }
                    | ("hgt", value) :: values -> values |> parsePass { passport with Height = Some value }
                    | ("hcl", value) :: values -> values |> parsePass { passport with HairColor = Some value }
                    | ("ecl", value) :: values -> values |> parsePass { passport with EyeColor = Some value }
                    | ("pid", value) :: values -> values |> parsePass { passport with PassportID = Some value }
                    | ("cid", value) :: values -> values |> parsePass { passport with CountryID = Some value }
                    | (k, v) :: _ -> failwithf "Invalid key %s with value %s" k v

                line.Split ' '
                |> Seq.choose (fun part ->
                    match part.Split (':', 2) with
                    | [| k; v  |] -> Some (k, v)
                    | _ -> None
                )
                |> Seq.toList
                |> parsePass empty

        let private mergeTo passport current =
            {
                BirthYear = passport.BirthYear |> Option.orElse current.BirthYear
                IssueYear = passport.IssueYear |> Option.orElse current.IssueYear
                ExpirationYear = passport.ExpirationYear |> Option.orElse current.ExpirationYear
                Height = passport.Height |> Option.orElse current.Height
                HairColor = passport.HairColor |> Option.orElse current.HairColor
                EyeColor = passport.EyeColor |> Option.orElse current.EyeColor
                PassportID = passport.PassportID |> Option.orElse current.PassportID
                CountryID = passport.CountryID |> Option.orElse current.CountryID
            }

        let validatePassports input =
            let rec countValid (p, acc) = function
                | [] -> p :: acc |> List.filter isValid |> List.length
                | "" :: lines -> lines |> countValid (empty, p :: acc)
                | line :: lines -> lines |> countValid (line |> parse |> mergeTo p, acc)

            input |> countValid (empty, [])

        let second input = 0

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

        let firstPuzzle =
            match input with
            | Input.HasOption "second-puzzle" _ -> false
            | _ -> true

        let handleResult f dayResult = result {
            match expected with
            | Some expected ->
                do! dayResult |> Assert.eq (f expected)
                return "Done"
            | _ ->
                return sprintf "Result value is %A" dayResult
        }

        match day with
        | 1 ->
            let! day1result =
                if firstPuzzle
                then inputLines |> Day1.tryFind2MatchingNumbers
                else inputLines |> Day1.tryFind3MatchingNumbers
                |> Result.ofOption "There are no numbers in the input which matches a criteria."

            return! handleResult int day1result
        | 2 ->
            let day2result =
                if firstPuzzle
                then inputLines |> Day2.countValidPasswords
                else inputLines |> Day2.countValidPasswordBySecondPolicy

            return! handleResult int day2result
        | 3 ->
            let day3result =
                if firstPuzzle
                then inputLines |> Day3.countTreesInPathByR3D1 |> int64
                else inputLines |> Day3.countTreesInPath

            return! handleResult int64 day3result
        | 4 ->
            let day4result =
                if firstPuzzle
                then inputLines |> Day4.validatePassports
                else inputLines |> Day4.second

            return! handleResult int day4result
        | day ->
            return! Error <| sprintf "Day %A is not ready yet." day
    })
