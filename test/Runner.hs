import Task1Suite (task1Checks, task1Tests)
import Task2Suite (task2Checks, task2Tests)
import Task3Suite (task3Checks, task3Tests)
import Task4Suite (task4Checks, task4Tests)
import Test.Tasty

main :: IO ()
-- main = defaultMain $ testGroup "" [checks, tests]
main = defaultMain $ testGroup "" [task2All]

task2All :: TestTree
task2All = testGroup "Task2 all" [task2Checks, task2Tests]

checks :: TestTree
checks =
  testGroup
    "Checks"
    [ task1Checks,
      task2Checks,
      task3Checks,
      task4Checks
    ]

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ task1Tests,
      task2Tests,
      task3Tests,
      task4Tests
    ]
