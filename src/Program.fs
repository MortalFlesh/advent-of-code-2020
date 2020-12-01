open System
open System.IO
open MF.ConsoleApplication
open MF.AdventOfCode
open MF.AdventOfCode.Console

[<EntryPoint>]
let main argv =
    consoleApplication {
        title AssemblyVersionInformation.AssemblyProduct
        info ApplicationInfo.MainTitle
        version AssemblyVersionInformation.AssemblyVersion

        command "advent:run" {
            Description = "Runs an advent-of-code application."
            Help = None
            Arguments = Command.AdventOfCode.args
            Options = [
                Option.noValue "second-puzzle" (Some "s") "Whether you are expecting a result of the second puzzle."
            ]
            Initialize = None
            Interact = None
            Execute = Command.AdventOfCode.execute
        }

        command "about" {
            Description = "Displays information about the current project."
            Help = None
            Arguments = []
            Options = []
            Initialize = None
            Interact = None
            Execute = Command.Common.about
        }
    }
    |> run argv
