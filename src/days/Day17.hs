module DAYS.Day17 (day17) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')
import Data.Bits (xor, (.|.), (.>>.), (.<<.))
import qualified Data.Bits as B
import Debug.Trace (trace)
import Data.Maybe (catMaybes)

type Registers = (Int, Int, Int)

type Program = Array Int Int

type InstructionPointer = Int

pComputerState :: Parser (Registers, Program)
pComputerState = (,) <$> pRegisters <*> (eol *> pProgram)
  where
    pRegisters :: Parser Registers
    pRegisters =
      (,,)
        <$> ((string "Register A: ") *> decimal <* eol)
        <*> ((string "Register B: ") *> decimal <* eol)
        <*> ((string "Register C: ") *> decimal <* eol)

    pProgram :: Parser Program
    pProgram = (string "Program: ") *> (arrayify1d <$> (sepBy decimal (char ',')))

arrayify1d :: [Int] -> Array Int Int
arrayify1d d = A.listArray (0, (length d) - 1) d

comboOp :: Int -> Registers -> Int
comboOp op (a, b, c) = case op of
  _ | op < 4 -> op
  4 -> a
  5 -> b
  6 -> c
  _ -> error "invalid combo operand"

part1 :: InstructionPointer -> (Registers, Program) -> [Int]
part1 i (regs, prog) = if i == length prog then [] else performInstr
  where
    (a, b, c) = regs
    performInstr = 
      case ((prog ! i), (prog ! (i + 1))) of
        --adv:
        (0, op) -> let a' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a',b,c), prog)
        --bxl:
        (1, op) -> let b' = xor b op in part1 (i+2) ((a,b',c),prog)
        --bsl:
        (2, op) -> let b' = (comboOp op regs) `mod` 8 in part1 (i+2) ((a,b',c),prog)
        --jnz:
        (3, op) -> if a == 0 then part1 (i+2) (regs, prog) else part1 op (regs, prog)
        --bxc
        (4, _) -> let b' = xor b c in part1 (i+2) ((a,b',c), prog) 
        --out
        (5, op) -> ((comboOp op regs) `mod` 8):part1 (i+2) ((a,b,c),prog)
        --bdv:
        (6, op) -> let b' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a,b',c), prog)
        --cdv:
        (7, op) -> let c' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a,b,c'), prog)
        _ -> error "invalid instruction"
  
bruteForce :: Int -> Int -> Int -> (Registers, Program) -> [Int]
bruteForce a target s (regs, prog) = 
  if s == 0 then [] else 
  if (take target $ part1 0 ((a,0,0), prog)) == (take target $ A.elems prog) 
    then a : bruteForce (a+1) target (s-1) (regs, prog)
    else bruteForce (a+1) target s (regs, prog)

part2 :: Int -> (Registers, Program) -> Int 
part2 j (regs, prog) = case part2' 1 j of 
  Just a -> a
  Nothing -> trace (show j) $ part2  (j+1) (regs, prog)
  where
    progLs = A.elems prog
    part2' :: Int -> Int -> Maybe Int
    part2' i a = if i == 11 then Just a else 
      case --trace ("recurse" ++ show recurse) 
          recurse of
        (x:xs) -> Just $ minimum (x:xs)
        _ -> Nothing
      where 
        positioniCandidates = map (\y -> a .|. (y .<<. ((i*3)+4)))  [0..15]

        possibleSeqs = filter (\y -> (take i (part1 0 ((y, 0, 0), prog))) == (take i progLs)) positioniCandidates

        seqResults = map (\y -> part1 0 ((y, 0, 0), prog)) positioniCandidates
        recurse = 
                  -- trace ("\na: " ++ show a ++ 
                  --       "\ni: " ++ show i ++ 
                  --        "\ncandidates: " ++ show positioniCandidates ++ 
                  --        "\nseq results: " ++ show (zip positioniCandidates seqResults) ++
                  --        "\npossible seqs: " ++ show possibleSeqs
                  --         ) $ 
                          catMaybes $ map (\y -> part2' (i+1) y ) possibleSeqs        

day17 :: IO (Int, Int)
day17 = do
  input <- readFile "src/inputs/day17.txt"
  let p1 = parse' input ((part1 0) <$> pComputerState) in print p1
  --let p2 = (parse' input ((bruteForce 0 6 10) <$> pComputerState)) in print p2
  --let p2 = (parse' input ((part2 0) <$> pComputerState)) in print p2
  pure $ (-1, -1)


-- [2,1,5,1,0,1,2,1,6]
-- 215101216
-- incorrect ^