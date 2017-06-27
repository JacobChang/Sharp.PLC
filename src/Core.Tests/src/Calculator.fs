module Tests
open PLC.Core
open Expecto

[<Tests>]
let tests =
  Calculator.test
  testList "Core.Calculator" [
    testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
      Expect.equal "abcdef" "abcdef" "These should equal"
  ]
