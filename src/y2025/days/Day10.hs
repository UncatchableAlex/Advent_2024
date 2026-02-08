  module Y2025.DAYS.Day10 (day10) where

  import Text.Megaparsec (sepBy, some, choice)
  import Text.Megaparsec.Char (char, eol, space)
  import Text.Megaparsec.Char.Lexer (decimal)
  import UTIL.Parsers (Parser, parse')
  import qualified Data.Map as M 
  import Data.Foldable (minimumBy)
  import Data.Array (Array, (!), (//))
  import qualified Data.Array as A
  import Data.Ord (comparing)
  import Data.List (transpose)
  import UTIL.Util (arrayify2d, powerset)
  import Data.Ratio ((%), numerator, denominator)
  import UTIL.LA (gaussREF, matvec)

  type Machine = (String, [[Int]], [Int])

  pInput :: Parser [Machine]
  pInput = sepBy pMachine eol
    where
      pMachine = (,,) <$> pLights <*> (some (pButton <* space)) <*> pJoltage 
      pLights = (char '[') *> (some $ choice [char '.', char '#']) <* (char ']' <* space)
      pButton = char '(' *> (sepBy decimal (char ',')) <* char ')'
      pJoltage = char '{' *> (sepBy decimal (char ',')) <* char '}'

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
  -- Observations:
  -- - ILP problem
  -- - 
  part2 :: [Machine] -> Int
  part2 machines = sum $ map fixMachine machines
    where

      changeType :: Int -> [Int] -> Rational
      changeType i button =  (fromIntegral $ fromEnum (i `elem` button)) % 1 

      fixMachine :: Machine -> Int
      fixMachine (_, buttons, dials) =
        let 
          n = length dials
          arr =  arrayify2d $ transpose ([[changeType i button | i <- [0..n-1]] | button <- buttons] ++ [map fromIntegral dials])
          arrInt = arrayify2d $ transpose ([[fromEnum $ i `elem` button | i <- [0..n-1]] | button <- buttons])        
          (ref, frees) = gaussREF arr
          x = backSub ref frees arrInt dials
        in 
          sum x


  -- Branch a prune a solution on the free variables using back substitution.
  -- See https://en.wikipedia.org/wiki/Triangular_matrix#Forward_substitution
  backSub :: Array (Int, Int) Rational -> [Int] -> Array (Int, Int) Int -> [Int] -> [Int]
  -- ref: REF matrix
  -- frees: our list of free variables
  -- buttonLengths: the number of dials each button presses
  -- dialSum: the sum of the dials
  backSub ref frees buttons dials = A.elems $ fix xInit m n bestInit
    where
      ((_,_),(m,n')) = A.bounds ref
      n = n'-1 -- ignore the augmented column
      sumDials = sum dials
      bestInit = A.listArray (0,0) [sumDials]
      xInit = A.listArray (0,n) $ replicate (n + 1) 0
      

      fix :: Array Int Int -> Int -> Int -> Array Int Int -> Array Int Int
      -- xs: our button presses so far
      -- i: the row of the suspected pivot button
      -- j: the button we are solving for
      -- best: the best result we have found so far
      fix xs i j best 
        -- immediately prune infeasible partial solutions
        | sumXs >= sumBest = best

        -- we found a solution on the free variables
        | i < 0 || j < 0 =  if (sumXs < sumBest) then xs else best

        -- if button j is free, we are going to have to brute force it
        | j `elem` frees = 
          let    
            -- where the dials are currently at with our partial solution       
            dialStatus = matvec buttons xs
            
            -- how much more each dial can turn
            dialCapacity = [d - ds | (d, ds, k) <- zip3 dials dialStatus [0..m], buttons!(k,j) == 1]
            
            -- find the value of the dial that constrains pressing the jth button. This
            -- will act as a bound on our brute force
            xjUpperBound = minimum dialCapacity

            step :: Array Int Int -> Int -> Array Int Int
            step topCandidate xj =
              let 
                xs' = xs // [(j,xj)]
                candidate = fix xs' i (j-1) topCandidate
              in snd $ minimum  [(sum candidate, candidate), (sum topCandidate, topCandidate)] 
          in 
            -- test every possible value for xj, accumulating the best solution we have found up
            -- to that point for pruning
            foldl' step best [0..xjUpperBound]
        | all (==0) [ref!(i,k) | k <- [0..n']] = fix xs (i-1) j best 
        
        -- pivot variable case. We can solve for this variable directly. For more info, see
        -- https://en.wikipedia.org/wiki/Triangular_matrix#Forward_substitution
        | otherwise = 
          let
            s = sum [(toInteger (xs!k) % 1) * ref!(i,k) | k <- [j+1..n]]
            xj = (ref!(i,n') - s) / ref!(i,j)
            xjInt = fromInteger $ numerator xj
            xs' = xs // [(j,xjInt)]
          in
            if denominator xj == 1 && xj >= 0 
              then fix xs' (i-1) (j-1) best
              else best  --if the solution for this variable is negative or a noninteger, prune
        where
          sumXs = sum xs
          sumBest = sum best
              
  day10 :: IO (Int, Int)
  day10 = do
    content <- readFile "src/y2025/inputs/day10.txt"
    let runPart part = parse' content $ part <$> pInput  
    pure (runPart part1, runPart part2)
