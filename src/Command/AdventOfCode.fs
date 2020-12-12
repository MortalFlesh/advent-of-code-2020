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

        let private parseStrict = function
            | null | "" -> failwith "[Logic] Empty line can not be parsed."
            | line ->
                let btw (min, max) (value: string) =
                    try int value >= min && int value <= max
                    with _ -> false

                let rec parsePass passport = function
                    | [] -> Some passport
                    | ("byr", Regex @"^(\d{4})$" [ value ]) :: values when value |> btw (1920, 2002) -> values |> parsePass { passport with BirthYear = Some value }
                    | ("iyr", Regex @"^(\d{4})$" [ value ]) :: values when value |> btw (2010, 2020) -> values |> parsePass { passport with IssueYear = Some value }
                    | ("eyr", Regex @"^(\d{4})$" [ value ]) :: values when value |> btw (2020, 2030) -> values |> parsePass { passport with ExpirationYear = Some value }
                    | ("hgt", Regex @"^(\d+)(cm|in)$" [ value; metric ]) :: values when (metric = "cm" && value |> btw (150, 193)) || (metric = "in" && value |> btw (59, 76)) -> values |> parsePass { passport with Height = Some value }
                    | ("hcl", Regex @"^(#[a-f\d]{6})$" [ value ]) :: values -> values |> parsePass { passport with HairColor = Some value }
                    | ("ecl", value) :: values when [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ] |> List.exists ((=) value) -> values |> parsePass { passport with EyeColor = Some value }
                    | ("pid", Regex @"^(\d{9})$" [ value ]) :: values -> values |> parsePass { passport with PassportID = Some value }
                    | ("cid", value) :: values -> values |> parsePass { passport with CountryID = Some value }
                    | (k, v) :: _ ->
                        // printfn "Invalid %s with %s" k v
                        None

                line.Split ' '
                |> Seq.choose (fun part ->
                    match part.Split (':', 2) with
                    | [| k; v  |] -> Some (k, v)
                    | _ -> None
                )
                |> Seq.toList
                |> parsePass empty

        let validatePassportsStrictly input =
            let rec countValid (p, acc) = function
                | [] -> p :: acc |> List.filter isValid |> List.length
                | "" :: lines -> lines |> countValid (empty, p :: acc)
                | line :: lines ->
                    lines |> countValid (
                        line |> parseStrict |> Option.map (mergeTo p) |> Option.defaultValue p,
                        acc
                    )

            input |> countValid (empty, [])

    [<RequireQualifiedAccess>]
    module Day5 =
        type private Seat = {
            Row: int
            Column: int
            Id: int
        }

        let private seatId { Id = id } = id
        let private newSeatId row column = row * 8 + column

        type private Range = {
            Min: int
            Max: int
        }

        let private parseSeat (value: string) =
            let rowValue = value.Substring(0, 7)
            let colValue = value.Substring(7, 3)

            // printfn "%s: %A[%d] | %A[%d]" value rowValue rowValue.Length colValue colValue.Length

            let half { Min = min; Max = max } =
                (min + max) / 2

            let rec parse (lower, upper) acc = function
                | [] -> failwithf "Empty input given"

                | [ l ] when l = lower -> acc.Min
                | [ u ] when u = upper -> acc.Max

                | l :: rest when l = lower ->
                    let acc = { acc with Max = acc |> half }
                    // printfn "%A -> (Min: %A; Max: %A)" lower acc.Min acc.Max
                    rest |> parse (lower, upper) acc

                | u :: rest when u = upper ->
                    let acc = { acc with Min = (acc |> half) + 1 }
                    // printfn "%A -> (Min: %A; Max: %A)" upper acc.Min acc.Max
                    rest |> parse (lower, upper) acc

                | invalid :: _ -> failwithf "Invalid input %A" invalid

            let rec parseRow = parse ('F', 'B') { Min = 0; Max = 127 }
            let rec parseColumn = parse ('L', 'R') { Min = 0; Max = 7 }

            let row = rowValue |> Seq.toList |> parseRow
            let column = colValue |> Seq.toList |> parseColumn

            // printfn "Row: %A | Column: %A" row column

            {
                Row = row
                Column = column
                Id = newSeatId row column
            }

        let findTheHighestSeatId input =
            input
            |> List.map (parseSeat >> seatId)
            |> List.max

        let findMySeat input =
            let occupiedSeatIds =
                input
                |> List.map (parseSeat >> seatId)

            [
                for row in 1..126 do    // rows minus very front/back
                    for column in 1..6 do   // there must be a left and right seat around me, so I'm not at the side
                        let id = newSeatId row column

                        if occupiedSeatIds |> List.exists ((=) id) |> not
                        then
                            let leftSeat = newSeatId row (column - 1)
                            let rightSeat = newSeatId row (column + 1)

                            if occupiedSeatIds |> List.exists ((=) leftSeat) && occupiedSeatIds |> List.exists ((=) rightSeat) then
                                yield id
            ]
            |> tee (List.length >> printfn "Found seats: %A")
            |> List.head

    [<RequireQualifiedAccess>]
    module Day6 =
        let countAnswersInGroups input =
            input
            |> String.concat "\n"
            |> String.split "\n\n"
            |> Seq.map (String.replace "\n" "" >> Seq.toList >> Seq.distinct)
            |> Seq.sumBy Seq.length

        let countAnswerEveryOneInTheGroupHas input =
            input
            |> String.concat "\n"
            |> String.split "\n\n"
            |> Seq.sumBy (fun group ->
                let persons =
                    group
                    |> String.split "\n"
                    |> Seq.map Set.ofSeq

                persons
                |> Seq.fold Set.intersect (persons |> Seq.head)
                |> Seq.length
            )

    [<RequireQualifiedAccess>]
    module Day7 =
        type private Color = {
            Name: string
            Content: string list
        }

        let private parseRuleColor = String.trim ' ' >> function
            | null | "" -> None
            | "no other bags" -> None
            | Regex @"^(\d+?) ([\w ]+?) bags?$" [ _count; color ] ->
                Some (color.Trim ' ')
            | _ -> None

        let private parseColorLine parseRule = function
            | null | "" -> None
            | Regex @"^([\w ]+?)bags contain(.+)\.$" [ color; contains ] ->
                Some (
                    color.Trim ' ',
                    contains.Split ',' |> Seq.toList |> List.choose parseRule
                )
            | _ -> None

        let private collectColors allColors =
            let allColorsMap =
                allColors
                |> Map.ofList

            let rec findAllColors (acc: Set<string>) = function
                | [] -> acc
                | color :: content ->
                    let currentColorColors =
                        match allColorsMap |> Map.tryFind color with
                        | Some directlyContains ->
                            directlyContains
                            |> findAllColors (acc |> Set.union (directlyContains |> Set.ofList))

                        | _ -> Set.empty

                    content |> findAllColors (acc |> Set.union currentColorColors)

            allColorsMap
            |> Map.map (fun color directlyContains -> directlyContains |> findAllColors (directlyContains |> Set.ofList))

        let countBagColorsWhichCanContainShinyGoldBag input =
            input
            |> List.choose (parseColorLine parseRuleColor)
            //|> tee (List.iter (fun (c, colors) -> printfn " - %s: %s // directly" c (colors |> String.concat ", ")))
            //|> tee (ignore >> printfn "    --- %A ---")
            |> collectColors
            |> Map.toList
            //|> tee (List.iter (fun (c, colors) -> printfn " * %s: %s // collected" c (colors |> String.concat ", ")))
            |> List.sumBy (fun (c, colors) ->
                if colors |> Set.contains "shiny gold" then 1
                else 0
            )

        type private Rule =
            | NoOtherBags
            | OtherBag of int * string

        let private parseRule = String.trim ' ' >> function
            | null | "" -> None
            | "no other bags" -> Some NoOtherBags
            | Regex @"^(\d+?) ([\w ]+?) bags?$" [ count; color ] ->
                Some ( OtherBag (int count, color.Trim ' ') )
            | _ -> None

        let private countBags wantedColor allColors =
            let allColorsMap =
                allColors
                |> Map.ofList

            let rec countAllBags acc = function
                | [] -> acc
                | NoOtherBags :: rules -> rules |> countAllBags acc
                | OtherBag (count, color) :: rules ->
                    let deep =
                        match allColorsMap |> Map.tryFind color with
                        | Some rules -> rules |> countAllBags 0
                        | _ -> 0

                    rules |> countAllBags (acc + deep * count + count)

            allColorsMap.[wantedColor] |> countAllBags 0

        let countAllBagsInShinyGoldBag input =
            input
            |> List.choose (parseColorLine parseRule)
            |> countBags "shiny gold"

    [<RequireQualifiedAccess>]
    module Day8 =
        type private Operation =
            | Acc of int
            | Jmp of int
            | Nop

        let private signed sign value =
            if sign = "+" then (int value)
            else -1 * (int value)

        let private parseOperation = function
            | null | "" -> None
            | Regex @"^(\w{3}) (\+|-)(\d+)$" [ operation; sign; value ] ->
                match operation with
                | "acc" -> Some (Acc (signed sign value))
                | "jmp" -> Some (Jmp (signed sign value))
                | "nop" -> Some (Nop)
                | _ -> None
            | _ -> None

        let rec private execute (operations: (int * Operation) list) (executed, acc) (i, operation) =
            if executed |> List.contains i then acc
            else
                let acc, nextOperation =
                    match operation with
                    | Acc value -> acc + value, i + 1
                    | Nop -> acc, i + 1
                    | Jmp value -> acc, i + value

                operations.[nextOperation] |> execute operations (i :: executed, acc)

        let runProgramWithoutLooping input =
            let operations =
                input
                |> List.choose parseOperation
                |> List.mapi (fun i operation -> i, operation)

            operations.Head |> execute operations ([], 0)

        type private Operation2 =
            | Acc of int
            | Jmp of int
            | Nop of int
            | Terminate

        let private parseOperation2 = function
            | null | "" -> None
            | Regex @"^(\w{3}) (\+|-)(\d+)$" [ operation; sign; value ] ->
                match operation with
                | "acc" -> Some (Acc (signed sign value))
                | "jmp" -> Some (Jmp (signed sign value))
                | "nop" -> Some (Nop (signed sign value))
                | _ -> None
            | _ -> None

        let private fixOperation = function
            | Nop value -> Jmp value
            | Jmp value -> Nop value
            | operation -> operation

        let rec private executeToEnd (operations: Map<int, Operation2>) (executed, acc) = function
            | _, Terminate -> Some acc
            | i, _ when executed |> List.contains i -> None
            | i, operation ->
                let acc, nextOperation =
                    match operation with
                    | Acc value -> acc + value, i + 1
                    | Nop _ -> acc, i + 1
                    | Jmp value -> acc, i + value
                    | invalid -> failwithf "Invalid operation %A" invalid

                (nextOperation, operations.[nextOperation]) |> executeToEnd operations (i :: executed, acc)

        let runProgramWithFixToTheEnd input =
            let operations =
                (input
                |> List.choose parseOperation2)
                @ [ Terminate ]
                |> List.mapi (fun i operation -> i, operation)

            let fixableOperations =
                operations
                |> List.choose (function
                    | (i, Nop _)
                    | (i, Jmp _) -> Some i
                    | _ -> None
                )

            let operationsMap = operations |> Map.ofList

            fixableOperations
            |> List.pick (fun fixable ->
                let fixedOperations =
                    operationsMap
                    |> Map.add fixable (operationsMap.[fixable] |> fixOperation)

                (0, fixedOperations.[0]) |> executeToEnd fixedOperations ([], 0)
            )

    [<RequireQualifiedAccess>]
    module Day9 =
        let private isValid (values: int64 list) number =
            seq {
                for a in 0 .. values.Length - 2 do
                    for b in a + 1 .. values.Length - 1 do
                        values.[a] + values.[b]
            }
            |> Seq.exists ((=) number)

        let findFirstNumberThatDoesntFollowTheRule input =
            let input = input |> List.map int64
            let preambleLength = if input |> List.length < 25 then 5 else 25

            input.[preambleLength..]
            |> List.mapi (fun i value -> i, value)
            |> List.find (fun (i, value) ->
                value
                |> isValid (input.[i..] |> List.take preambleLength)
                |> not
            )
            |> snd

        let private findContigousSetToMatchNumber number (input: _ list) =
            let input = input |> List.mapi (fun i value -> i, value)

            let rec sum start acc = function
                | [] -> None
                | (_, overLimit) :: _ when (overLimit + acc) > number -> None
                | (i, correct) :: _ when (correct + acc) = number -> Some (input.[start .. i] |> List.map snd)
                | (_, value) :: values -> values |> sum start (acc + value)

            input
            |> List.pick (fun (i, _) -> sum i (int64 0) input.[i..])

        let breakTheEncryption input =
            let invalidNumber = input |> findFirstNumberThatDoesntFollowTheRule
            let input = input |> List.map int64

            let (min, max) =
                let setOfNumbers = input |> findContigousSetToMatchNumber invalidNumber
                (setOfNumbers |> List.min), (setOfNumbers |> List.max)

            min + max

    [<RequireQualifiedAccess>]
    module Day10 =
        let findJoltageChain input =
            let input =
                input
                |> List.map int
                |> List.sort

            let rec tryFindJoltage (differences: int list) = function
                | [] -> []

                | [ last ] -> 3 :: differences

                | value :: next :: values when value + 1 = next -> next :: values |> tryFindJoltage (1 :: differences)
                | value :: next :: values when value + 2 = next -> next :: values |> tryFindJoltage (2 :: differences)
                | value :: next :: values when value + 3 = next -> next :: values |> tryFindJoltage (3 :: differences)

                | _ -> []

            let joltageSequenceDifferences =
                0 :: input
                |> tryFindJoltage []

            let one, two, three =
                match joltageSequenceDifferences |> List.countBy id |> List.sortBy fst with
                | [ (1, ones) ] -> (ones, 0, 0)
                | [ (2, twos) ] -> (0, twos, 0)
                | [ (3, threes) ] -> (0, 0, threes)
                | [ (1, ones); (2, twos) ] -> (ones, twos, 0)
                | [ (1, ones); (3, threes) ] -> (ones, 0, threes)
                | [ (2, twos); (3, threes) ] -> (0, twos, threes)
                | [ (1, ones); (2, twos); (3, threes) ] -> (ones, twos, threes)
                | _ -> 0, 0, 0

            printfn "1: %A | 2: %A | 3: %A" one two three

            one * three

        let findAllPossibleJoltageChains input =
            let input =
                input
                |> List.map int
                |> List.sort

            let input = 0 :: input
            let lastValue = input |> List.max

            let rec tryFindJoltage = function
                | [] -> None

                | [ last ] when last = lastValue -> Some 1

                | value :: next :: values when value + 1 = next -> next :: values |> tryFindJoltage
                | value :: next :: values when value + 2 = next -> next :: values |> tryFindJoltage
                | value :: next :: values when value + 3 = next -> next :: values |> tryFindJoltage

                | _ -> None

            let inputIndexes = [0 .. input.Length - 1]

            // skip bude list indexu, ktere preskocim
            // pro [ 1, 2, 3, 4 ] (L=4) by to bylo
            // * 0:  [ []  []  []  [] ]
            // * 1:  [ [0] [1] [2] [3] ]
            // * 2:  [ [0, 1] [0, 2] [0, 3] [1, 2] [1, 3] [2, 3] ]
            // * 3:  [ [0, 1, 2] [0, 1, 3] [0, 2, 3] ... ]
            // * L-1 !

            let skippedIndexes =
                let firstPossibleIdxToSkip = 1
                let len = input.Length - 1 // last number cannot be replaced

                seq {
                    yield seq { [] }    // original values (skip none)

                    for size in 1 .. len - 1 do
                        if size = 1 then yield seq { firstPossibleIdxToSkip .. len - 1 } |> Seq.map List.singleton

                        if size = 2 then
                            yield seq {
                                for i in firstPossibleIdxToSkip .. len - 2 do
                                    for j in i + 1 .. len - 1 do
                                        yield [ i; j ]
                            }

                        if size = 3 then
                            yield seq {
                                for i in firstPossibleIdxToSkip .. len - 3 do
                                    for j in i + 1 .. len - 2 do
                                        for k in j + 1 .. len - 1 do
                                            yield [ i; j; k ]
                            }

                        if size = 4 then
                            yield seq {
                                for i in firstPossibleIdxToSkip .. len - 4 do
                                    for j in i + 1 .. len - 3 do
                                        for k in j + 1 .. len - 2 do
                                            for l in k + 1 .. len - 1 do
                                                yield [ i; j; k; l ]
                            }

                        if size = 5 then
                            yield seq {
                                for i in firstPossibleIdxToSkip .. len - 5 do
                                for j in i + 1 .. len - 4 do
                                for k in j + 1 .. len - 3 do
                                for l in k + 1 .. len - 2 do
                                for m in l + 1 .. len - 1 do
                                    yield [ i; j; k; l; m ]
                            }

                        if size = 6 then
                            yield seq {
                                for i in firstPossibleIdxToSkip .. len - 6 do
                                for j in i + 1 .. len - 5 do
                                for k in j + 1 .. len - 4 do
                                for l in k + 1 .. len - 3 do
                                for m in l + 1 .. len - 2 do
                                for n in m + 1 .. len - 1 do
                                    yield [ i; j; k; l; m; n ]
                            }

                        // todo - this would go to the max(size) with a dynamic number of inside for loops...
                }
                |> Seq.concat
                |> Seq.map Set.ofList
                |> Seq.distinct
                |> Seq.sortBy Set.count

            (* printfn "L: %A" input.Length
            skippedIndexes
            //|> Seq.filter (Set.count >> (=) 1)
            |> Seq.iter (printfn "* %A") *)

            let skip (skipped: Set<int>) list =
                list
                |> List.mapi (fun i value -> if skipped.Contains i then None else Some value)
                |> List.choose id

            skippedIndexes
            |> Seq.choose (fun skipped ->
                input
                |> skip skipped
                // |> tee (printfn "input: %A with skipped %A -> %A" input skipped)
                |> tryFindJoltage
                // |> tee (Option.isSome >> printfn " -> is valid %A")
                // |> tee (function None -> () | Some _ -> printfn "input: %A with skipped %A -> %A" input skipped (input |> skip skipped |> List.map string |> String.concat ", "))
            )
            |> Seq.sum

    [<RequireQualifiedAccess>]
    module Day11 =
        type private Place =
            | Floor
            | EmptySeat
            | OccupiedSeat

        let private parse = function
            | '.' -> Floor
            | 'L' -> EmptySeat
            | '#' -> OccupiedSeat
            | invalid -> failwithf "Invalid %A" invalid

        let private parseLine (line: string) =
            line |> Seq.map parse |> Seq.toList

        let private place = function
            | Floor -> "."
            | EmptySeat -> "L"
            | OccupiedSeat -> "#"

        type private Boat = (Place list) list

        let private isEmpty = function
            | Floor | EmptySeat -> 1
            | _ -> 0

        let private isOccupied = function
            | OccupiedSeat -> Some OccupiedSeat
            | _ -> None

        let private occupiedAdjacentPlaces (places: Boat) row col =
            //printfn "For [%A][%A]\n-------------" row col
            seq {
                for c in (col - 1 |> max 0) .. (col + 1 |> min (places.[row].Length - 1)) do
                    if row > 0 then
                        //printf "up [%A][%A]" (row-1) c
                        places.[row-1].[c] |> isOccupied
                        //|> tee (printfn " -> %A")

                    if c <> col then
                        //printf "at [%A][%A]" (row) c
                        places.[row].[c] |> isOccupied
                        //|> tee (printfn " -> %A")

                    if row < places.Length - 1 then
                        //printf "do [%A][%A]" (row+1) c
                        places.[row+1].[c] |> isOccupied
                        //|> tee (printfn " -> %A")
            }

        let private round findOccupiedSeats numberOfOccupiedSeats (places: Boat): Boat =
            [
                for row in 0 .. places.Length - 1 do
                    [
                        for col in 0 .. places.[row].Length - 1 do
                            yield
                                match places.[row].[col] with
                                | Floor -> Floor

                                | EmptySeat ->
                                    findOccupiedSeats places row col
                                    |> Seq.tryPick id
                                    |> Option.bind (fun _ -> Some EmptySeat)
                                    |> Option.defaultValue OccupiedSeat
                                    // |> tee (printfn "Empty at [%A][%A] -> %A" row col)

                                | OccupiedSeat ->
                                    let mutable acc = 0

                                    findOccupiedSeats places row col
                                    |> Seq.tryPick (function
                                        | Some OccupiedSeat ->
                                            acc <- acc + 1

                                            if acc >= numberOfOccupiedSeats then Some EmptySeat
                                            else None
                                        | _ -> None
                                    )
                                    |> Option.defaultValue OccupiedSeat
                                    // |> tee (printfn "Occupied at [%A][%A] -> %A" row col)
                    ]
            ]

        let countOccupiedSeats input =
            // let mutable i = 0
            let rec run (previous: Boat) (current: Boat) =
                (* i <- i + 1
                printfn "%s" (String.replicate (current.[0].Length) "_")
                printfn " Round: %A" i
                printfn "%s" (String.replicate (current.[0].Length) "-")
                current
                |> List.iter (List.map place >> String.concat "" >> printfn "%s") *)

                if previous = current
                    then
                        current
                        |> List.concat
                        |> List.sumBy (function
                            | OccupiedSeat -> 1
                            | _ -> 0
                        )
                    else
                        current
                        |> round occupiedAdjacentPlaces 4
                        |> run current

            input
            |> List.map parseLine
            |> run []

        type private Seat =
            | OutOfBoat
            | Occupied
            | Empty

        let private isOccupiedSeat = function
            | Occupied -> Some OccupiedSeat
            | _ -> None

        let private checkPlace direction (places: Boat) row col =
            if row < 0 || col < 0 || row >= places.Length || col >= places.[0].Length then
                // printfn " * %s [%A][%A] -> out of boat" direction row col
                Some OutOfBoat
            else
                // printf " * %s [%A][%A]" direction row col
                match places.[row].[col] with
                | EmptySeat -> Some Empty
                | OccupiedSeat -> Some Occupied
                | Floor -> None
                // |> tee (printfn " -> %A")

        let private occupiedPlacesOnSight (places: Boat) row col =
            // printfn "\nFor [%A][%A] %A\n-------------" row col places.[row].[col]
            seq {
                // let leftUp =
                seq {
                    for i in [1 .. (row |> max col)] do
                        let r = row - i
                        let c = col - i

                        checkPlace "LU" places r c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "LU for [%A][%A]: %A" row col)

                //let up =
                seq {
                    for i in [1 .. row] do
                        let r = row - i

                        checkPlace "U" places r col
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "U  for [%A][%A]: %A" row col)

                //let rightUp =
                seq {
                    for i in [1 .. ((row |> max col))] do
                        let r = row - i
                        let c = col + i

                        checkPlace "RU" places r c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "RU for [%A][%A]: %A" row col)

                // let left =
                seq {
                    for i in [1 .. col] do
                        let c = col - i

                        checkPlace "L" places row c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "L for [%A][%A]: %A" row col)

                //let right =
                seq {
                    for i in [1 .. places.[0].Length] do
                        let c = col + i

                        checkPlace "R" places row c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "R  for [%A][%A]: %A" row col)

                // let leftDown =
                seq {
                    for i in [1 .. ((places.Length |> max places.[0].Length))] do
                        let r = row + i
                        let c = col + i

                        checkPlace "LD" places r c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "LD for [%A][%A]: %A" row col)

                //let down =
                seq {
                    for i in [1 .. places.Length - 1] do
                        let r = row + i

                        checkPlace "D" places r col
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "D  for [%A][%A]: %A" row col)

                // let rightDown =
                seq {
                    for i in [1 .. ((places.Length |> max places.[0].Length))] do
                        let r = row + i
                        let c = col - i

                        checkPlace "RD" places r c
                }
                |> Seq.tryPick id
                |> Option.bind isOccupiedSeat
                // |> tee (printfn "RD for [%A][%A]: %A" row col)
            }

        let countOccupiedSeatsOnSight input =
            // let mutable i = 0
            let rec run (previous: Boat) (current: Boat) =
                //i <- i + 1
                (* printfn "%s" (String.replicate (current.[0].Length) "_")
                printfn " Round: %A" i
                printfn "%s" (String.replicate (current.[0].Length) "-")
                current
                |> List.iter (List.map place >> String.concat "" >> printfn "%s") *)

                if previous = current
                    then
                        current
                        |> List.concat
                        |> List.sumBy (function
                            | OccupiedSeat -> 1
                            | _ -> 0
                        )
                    else
                        current
                        |> round occupiedPlacesOnSight 5
                        |> run current

            input
            |> List.map parseLine
            |> run []

    [<RequireQualifiedAccess>]
    module Day12 =
        type private Direction =
            | North
            | East
            | South
            | West

        type private ManhattanDistance = {
            EastWest: int
            NorthSouth: int
        }

        type private Turn =
            | Left
            | Right

        let private turn = function
            | Left, North -> West
            | Left, East -> North
            | Left, South -> East
            | Left, West -> South
            | Right, North -> East
            | Right, East -> South
            | Right, South -> West
            | Right, West -> North

        let private turnBoat degrees turnDirection current =
            [1 .. int degrees / 90]
            |> List.fold (fun acc _ -> turn (turnDirection, acc)) current

        let private format = function
            | North -> "N"
            | East -> "E"
            | South -> "S"
            | West -> "W"

        let countManhattanDistance input =
            let rec sail (current, acc) input =
                (* printfn "Facing: %A | %s: %A | %s: %A"
                    current
                    (if acc.EastWest >= 0 then "E" else "W")
                    (abs acc.EastWest)
                    (if acc.NorthSouth >= 0 then "N" else "S")
                    (abs acc.NorthSouth)
                printf " * %A -> " (input |> List.tryHead) *)

                match input with
                | [] -> (abs acc.EastWest) + (abs acc.NorthSouth)

                | Regex @"N(\d+)" [ number ] :: rest ->
                    rest |> sail (current, { acc with NorthSouth = acc.NorthSouth + (int number) })

                | Regex @"E(\d+)" [ number ] :: rest ->
                    rest |> sail (current, { acc with EastWest = acc.EastWest - (int number) })

                | Regex @"S(\d+)" [ number ] :: rest ->
                    rest |> sail (current, { acc with NorthSouth = acc.NorthSouth - (int number) })

                | Regex @"W(\d+)" [ number ] :: rest ->
                    rest |> sail (current, { acc with EastWest = acc.EastWest + (int number) })

                | Regex @"F(\d+)" [ number ] :: rest ->
                    sprintf "%s%s" (current |> format) number :: rest |> sail (current, acc)

                | Regex @"L(\d+)" [ degrees ] :: rest ->
                    rest |> sail (current |> turnBoat degrees Left, acc)

                | Regex @"R(\d+)" [ degrees ] :: rest ->
                    rest |> sail (current |> turnBoat degrees Right, acc)

                | invalid :: _ -> failwithf "Invalid input %A" invalid

            input
            |> sail (East, { EastWest = 0; NorthSouth = 0 })

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
                else inputLines |> Day4.validatePassportsStrictly

            return! handleResult int day4result
        | 5 ->
            let day5result =
                if firstPuzzle
                then inputLines |> Day5.findTheHighestSeatId
                else inputLines |> Day5.findMySeat

            return! handleResult int day5result
        | 6 ->
            let day6result =
                if firstPuzzle
                then inputLines |> Day6.countAnswersInGroups
                else inputLines |> Day6.countAnswerEveryOneInTheGroupHas

            return! handleResult int day6result
        | 7 ->
            let day7result =
                if firstPuzzle
                then inputLines |> Day7.countBagColorsWhichCanContainShinyGoldBag
                else inputLines |> Day7.countAllBagsInShinyGoldBag

            return! handleResult int day7result
        | 8 ->
            let day8result =
                if firstPuzzle
                then inputLines |> Day8.runProgramWithoutLooping
                else inputLines |> Day8.runProgramWithFixToTheEnd

            return! handleResult int day8result
        | 9 ->
            let day9result =
                if firstPuzzle
                then inputLines |> Day9.findFirstNumberThatDoesntFollowTheRule
                else inputLines |> Day9.breakTheEncryption

            return! handleResult int64 day9result
        | 10 ->
            let day10result =
                if firstPuzzle
                then inputLines |> Day10.findJoltageChain
                else inputLines |> Day10.findAllPossibleJoltageChains

            return! handleResult int day10result
        | 11 ->
            let day11result =
                if firstPuzzle
                then inputLines |> Day11.countOccupiedSeats
                else inputLines |> Day11.countOccupiedSeatsOnSight

            return! handleResult int day11result
        | 12 ->
            let day12result =
                if firstPuzzle
                then inputLines |> Day12.countManhattanDistance
                else inputLines |> Day12.second

            return! handleResult int day12result
        | day ->
            return! Error <| sprintf "Day %A is not ready yet." day
    })
