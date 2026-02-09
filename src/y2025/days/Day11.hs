module Y2025.DAYS.Day11 (day11) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Text.Megaparsec (chunk, sepBy, some)
import Text.Megaparsec.Char (eol, hspace1, lowerChar)
import UTIL.Parsers (Parser, parse')

pInput :: Parser [(String, [String])]
pInput = sepBy pOutput eol
  where
    pOutput :: Parser (String, [String])
    pOutput = (,) <$> ((some lowerChar) <* chunk ": ") <*> (sepBy (some lowerChar) hspace1)

-- Our memoization invariant will be "how many paths from this node exist to the end node".
dist :: String -> String -> [(String, [String])] -> Int
dist from to devices = fst $ dfs M.empty from
  where
    forward = M.fromList devices
    dfs :: Map String Int -> String -> (Int, Map String Int)
    dfs visited s
      -- if we have been there, we have discovered visited[s] new paths to the end
      | s `M.member` visited = (visited ! s, visited)
      -- if we have found a new path to the end, add 1 to the path count
      | s == to = (1, visited)
      -- if we are at an undiscovered node, track how many paths exist. Keep the visited memo global
      -- across sibling paths with a fold
      | otherwise = foldl' step (0, visited) $ forward ! s
      where
        step (n, visited') s' =
          let (n', visited'') = dfs visited' s'
           in (n + n', M.insert s (n + n') visited'')


part2 :: [(String, [String])] -> Int
part2 devices = (svrdac * dacfft * fftout) + (svrfft * fftdac * dacout)
  where
    devices' = ("out", []) : devices
    svrdac = dist "svr" "dac" devices'
    svrfft = dist "svr" "fft" devices'
    dacfft = dist "dac" "fft" devices'
    fftdac = dist "fft" "dac" devices'
    fftout = dist "fft" "out" devices'
    dacout = dist "dac" "out" devices'

day11 :: IO (Int, Int)
day11 = do
  content <- readFile "src/y2025/inputs/day11.txt"
  let runPart part = parse' content $ part <$> pInput
  pure (runPart (dist "you" "out"), runPart part2)

-- IMPORTANT NOTE: We are guaranteed that any valid path does not visit the same node twice. This because no cycle
-- no cycle exists in the graph. If a cycle were to exist along the path from "you" to "out", then there necessarily
-- must be an infinite number of solutions.

-- Proof: Suppose, we find some path from "you" to "out", P: you -> ... -> c -> ... -> c -> out, which visits device c
-- twice. I can generate arbitrary P_i for any positive integer i, by creating an identical path which visits c i times.
