module Y2025.DAYS.Day10 (day10) where

import Text.Megaparsec (sepBy, some, choice)
import Text.Megaparsec.Char (char, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import qualified Data.Map as M 
import Debug.Trace (trace)
import Data.Foldable (minimumBy, maximumBy)
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import Data.Ord (comparing)
import Data.List (sortBy, intersect)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S 
import UTIL.Util (count)


type Machine = (String, [[Int]], [Int])

pInput :: Parser [Machine]
pInput = sepBy pMachine eol
  where
    pMachine = (,,) <$> pLights <*> (some (pButton <* space)) <*> pJoltage 
    pLights = (char '[') *> (some $ choice [char '.', char '#']) <* (char ']' <* space)
    pButton = char '(' *> (sepBy decimal (char ',')) <* char ')'
    pJoltage = char '{' *> (sepBy decimal (char ',')) <* char '}'


powerset :: [[Int]] -> [[[Int]]]
powerset [] = [[]]
powerset (x:xs) =
  let ps = powerset xs
  in map (x:) ps ++ ps

-- OBSERVATIONS:
--  - there is no use in pressing a button twice. I could prove it... maybe
--  - this means that we can brute force on the powerset of available buttons
--  - we can calculate the net effect of pressing a combo of buttons by counting how many times 
--    each light is activated and modding by 2. This is what the lightsActivated function does
part1 :: [Machine] -> Int
part1 machines = sum $ map fixMachine machines
  where
    fixMachine (lights, buttons, _) = length $ minimumBy (comparing length) correct
      where
        correct = map fst $ filter ((== lightIdxs) . snd) (zip ps combos)
        ps = powerset buttons

        combos :: [[Int]]
        combos = map (presses . concat) ps

        presses :: [Int] -> [Int]
        presses combo = lightsActivated $ map (\x -> (x,1::Int)) combo

        lightsActivated :: [(Int, Int)] -> [Int]
        lightsActivated combo = M.keys $ M.filter ((/= 0) . (`mod` 2)) $ M.fromListWith (+) combo

        lightIdxs = [i | i <- [0..length lights -1], lights !! i == '#']

part3 :: [Machine] -> Int
part3 machines = sum $ map fixMachine machines
  where
    fixMachine :: Machine -> Int
    fixMachine (_, buttons, dials) =
      let 
        dialArr = A.listArray (0, length dials - 1) dials
        sortedButtons = sortBy (comparing $ negate . length) buttons
        res = dfs dialArr (sum dials) 0 sortedButtons
        --buttonMap = 
        -- upperBound = case greedySolver2 dialArr sortedButtons of
        --   Just x -> x
        --   Nothing -> -1

      in
        trace ("fixing " ++ (show (buttons, dials))) res --trace ("score to beat: " ++ (show upperBound)) res
        --upperBound

    dfs :: Array Int Int -> Int -> Int -> [[Int]] -> Int
    dfs dials best depth buttons
      | all (== 0) dials = trace (show (best, depth)) $ min best depth -- success base case 
      | any (< 0) dials = best  -- failure base case 1
      | (not . null) feasibleButtons && constraints && feasible0 && feasible1 = recurse
      | otherwise = best
      where
        recurse = foldl' step best [0..length feasibleButtons-1]
        nonzeroDials = [i | i <- [0..length dials - 1], dials ! i > 0]
        feasibleButton dials' button = and [dials' ! x > 0 | x <- button]
        associatedButtons1 dial = filter (elem dial) feasibleButtons -- get all the buttons associated with a given dial
        associatedButtons2 dial1 dial2 = filter (\x -> dial1 `elem` x || dial2 `elem` x) feasibleButtons -- get all the buttons associated with a given dial
        -- this indicates that there is still some way to decrement every nonzero dial
        feasible0 = (nonzeroDials `intersect` concat feasibleButtons ) == nonzeroDials
        feasible1 = 
          let 
            res = --greedySolver2 dials 3 $ associatedButtons 3
              [singleDialCap dials dial $ associatedButtons1 dial | dial <- nonzeroDials]
          in
            trace (show res) $ and res
           -- and res

        allPairs ls = [(a,b) | (a,i) <- zip ls [(0::Int)..], (b,j) <- zip ls [0..], i < j]

        feasible2 = 
          let 
            res = --greedySolver2 dials 3 $ associatedButtons 3
              [doubleDialCap dials (a, b) $ associatedButtons2 a b | (a,b) <- allPairs nonzeroDials]
          in
            trace (show $ filter (not . fst) $ zip res $ allPairs nonzeroDials) $ and res
            --and res


        scoreDial i = negate $ (dials ! i) `div` count (elem i) feasibleButtons
        scoreButton = sum . (map scoreDial)
        feasibleButtons =  filter (feasibleButton dials) buttons
        feasibleButtonsOrdered = sortBy (comparing $ negate . length) feasibleButtons
        biggestFeasibleButton = length $ maximumBy (comparing length) feasibleButtons 

        constraints = 
          let 
          --  lb1 = depth + lowerBound dials 
            lb2 = depth + (sum dials `div` biggestFeasibleButton)
            lb3 = depth + maximum dials
          in max lb2 lb3 < best
            -- if depth `mod` 5 == 0 
            --   then trace (show (depth, lb2, lb3, best)) max lb2 lb3 < best
            --   else max lb2 lb3 < best
        -- best = 
        --   case greedySolver2 dials feasibleButtonsOrdered of
        --   Just x -> trace("GREEDY SOL: " ++ show (depth + x)) $ min best (depth + x)
        --   Nothing -> trace ("No greedy sol") $ best 


        lowerBound :: Array Int Int -> Int
        lowerBound dials'
          | bestPresses == 0 = 0
          | otherwise = lowerBound dials'' + bestPresses
          where
            presses b = minimum $ map ((!) dials') b
            bestButton = maximumBy (comparing presses) buttons
            bestPresses = presses bestButton
            dials'' = dials' // [(x, (dials' ! x) - bestPresses) | x <- bestButton]

        --biggestFeasibleButton = length $ maximumBy (comparing length) feasibleButtons 
        --lowerBoundHeuristic1 = depth + (sum dials `div` biggestFeasibleButton) <= best
        --lowerBoundHeuristic2 = depth + maximum dials <= best
        step best' i = 
          case drop i feasibleButtonsOrdered of
            [] -> best
            (b:bs) ->
              let
                dials' = dials // [(j, (dials ! j) - 1) | j <- b]
                valid = all (>= 0) dials'
              in
                if valid then dfs dials' best' (depth + 1) (b:bs) else best
          


singleDialCap :: Array Int Int -> Int -> [[Int]] -> Bool
singleDialCap dials target buttons = any (valid dials) $ powerset buttons
  where
    valid :: Array Int Int -> [[Int]] -> Bool
    valid dials' (b:bs) =
      let 
        presses = minimum $ map ((!) dials') b 
        dials'' = dials' // [(x, (dials' ! x) - presses) | x <- b]
      in 
        if (dials' ! target) == 0 
          then True 
          else --trace ((show target) ++ "  " ++ (show $ A.elems dials') ++ "  " ++ (show $ A.elems dials'')) 
            valid dials'' bs
    valid dials' [] = (dials' ! target) == 0



day10 :: IO (Int, Int)
day10 = do
  content <- readFile "src/y2025/inputs/day10.txt"
  let runPart part = parse' content $ part <$> pInput  
  pure (runPart part1, runPart part3)


-- 70051 too high
-- 39110 too high