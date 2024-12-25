{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module TESTS.PQueueTests (tests) where

import Data.List (sort)
import Test.QuickCheck ((===), Property, quickCheck, property, counterexample, (.&&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import UTIL.PQueue (PQueue, merge, pop, push, empty, fromList, extractAll, popIdentical)
import Debug.Trace (trace)


-- Property: After inserting elements, extracting them should yield sorted keys
prop_insertExtractSorted :: [(Int, Char)] -> Property
prop_insertExtractSorted kvs =
  let q = fromList kvs
      extracted = map fst $ extractAll q
   in trace (show kvs) $ extracted === sort (map fst kvs)

-- Property: Merging two queues should preserve all elements
prop_mergePreservesElements :: [(Int, String)] -> [(Int, String)] -> Property
prop_mergePreservesElements xs ys =
  let q1 = foldl (\q (k, v) -> push (k,v) q) empty xs
      q2 = foldl (\q (k, v) -> push (k,v) q) empty ys
      merged = merge q1 q2
      extractedKeys = sort $ map fst $ extractAll merged
      originalKeys = sort $ map fst $ xs ++ ys
   in extractedKeys === originalKeys


prop_popIdentical :: Property
prop_popIdentical = 
  let 
    -- Create a queue with some duplicate keys
    q = foldl (\acc (k,v) -> push (k,v) acc) empty ([
        (1, "a"),
        (1, "b"),
        (1, "c"),
        (2, "d"),
        (3, "e")
      ] :: [(Int, String)])
    q' = popIdentical q
  in
    case q' of
      Nothing -> property False -- Should not happen with non-empty queue
      Just ((k,v), q'') -> 
        case popIdentical q'' of
          Nothing -> property False
          Just ((k',v'), _) ->
            -- Should get the first non-1 key
           (k,v,k',v') === (1,"a",2,"d")

-- Unit tests
tests :: TestTree
tests =
  testGroup
    "p queue"
    [ testCase "empty queue has no elements" $ Nothing @?= (pop (empty :: PQueue Int Int)),
      testCase "single element queue" $ [(1, "one")] @?= (extractAll $ push (1, "one") (empty :: PQueue Int String)),
      testCase  "multiple elements are sorted" $ (
        let 
          eles = [(2, "two"), (1, "one"), (3, "three")]
          f q tup = push tup q
        in 
          extractAll $ foldl f (empty :: PQueue Int String) eles)
          @?=  [(1, "one"), (2, "two"), (3, "three")],
      testCase "merge two queues" $
          [(1, "one"), (2, "two"), (3, "three"), (4, "four")] @?=
          ( extractAll $
              merge
                (foldl (\q (k, v) -> push (k,v) q) (empty :: PQueue Int String) [(2, "two"), (1, "one")])
                (foldl (\q (k, v) -> push (k,v) q) (empty :: PQueue Int String) [(4, "four"), (3, "three")])
          )
    ]

-- main :: IO ()
-- main = do
--     putStrLn "Running QuickCheck tests..."
--     quickCheck prop_insertExtractSorted
--     quickCheck prop_mergePreservesElements
--     putStrLn "\nRunning unit tests..."
--     return ()
