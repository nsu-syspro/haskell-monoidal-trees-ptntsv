import Data.Data (Proxy (Proxy), Typeable)
import Data.Maybe (fromMaybe)
import Task1Suite (task1Checks, task1Tests)
import Task2Suite (task2Checks, task2Tests)
import Task3Suite (task3Checks, task3Tests)
import Task4Suite (task4Checks, task4Tests)
import Test.Tasty
import Test.Tasty.Options (IsOption (..), OptionDescription (..))

newtype TaskOpt = TaskOpt {getTaskOpt :: Maybe String}
  deriving (Eq, Ord, Show, Typeable)

instance IsOption TaskOpt where
  defaultValue = TaskOpt Nothing

  parseValue s = Just (TaskOpt (Just s))

  optionName = pure "task"
  optionHelp = pure "Run tests for a given task"

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy TaskOpt)] : defaultIngredients)
    ( askOption $ \(TaskOpt task) ->
        case task of
          Just content -> askSuite content
          _ -> testGroup "all" allSuites
    )

askSuite :: String -> TestTree
askSuite name = testGroup name (fromMaybe allSuites $ lookup name suitesByTask)

allSuites :: [TestTree]
allSuites = concatMap snd suitesByTask

suitesByTask :: [(String, [TestTree])]
suitesByTask =
  [ ("Task1", [task1Tests, task1Checks]),
    ("Task2", [task2Tests, task2Checks]),
    ("Task3", [task3Tests, task3Checks]),
    ("Task4", [task4Tests, task4Checks])
  ]
