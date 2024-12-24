import TESTS.PQueueTests (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "APL"
      [ tests ]