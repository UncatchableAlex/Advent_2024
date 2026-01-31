module Y2024.DAYS.Day17 (day17) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Bits (xor, (.<<.), (.|.))
import Data.Maybe (catMaybes)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import UTIL.Parsers (Parser, parse')

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
        -- adv:
        (0, op) -> let a' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a', b, c), prog)
        -- bxl:
        (1, op) -> let b' = xor b op in part1 (i + 2) ((a, b', c), prog)
        -- bsl:
        (2, op) -> let b' = (comboOp op regs) `mod` 8 in part1 (i + 2) ((a, b', c), prog)
        -- jnz:
        (3, op) -> if a == 0 then part1 (i + 2) (regs, prog) else part1 op (regs, prog)
        -- bxc
        (4, _) -> let b' = xor b c in part1 (i + 2) ((a, b', c), prog)
        -- out
        (5, op) -> ((comboOp op regs) `mod` 8) : part1 (i + 2) ((a, b, c), prog)
        -- bdv:
        (6, op) -> let b' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a, b', c), prog)
        -- cdv:
        (7, op) -> let c' = a `div` (2 ^ comboOp op regs) in part1 (i + 2) ((a, b, c'), prog)
        _ -> error "invalid instruction"

-- do bfs on output right to left
part2 :: (Registers, Program) -> Int
part2 (_, prog) = case part2' (length prog - 1) 0 of
  Just a -> a
  Nothing -> error "no quine detected"
  where
    progLs = A.elems prog
    part2' :: Int -> Int -> Maybe Int
    part2' i a =
      if i == -1
        then Just a
        else case recurse of
          (x : xs) -> Just $ minimum (x : xs)
          _ -> Nothing
      where
        positioniCandidates = map (\y -> a .|. (y .<<. (i * 3))) [0 .. 7]
        validSeq y = (drop i (part1 0 ((y, 0, 0), prog))) == (drop i progLs)
        possibleSeqs = filter validSeq positioniCandidates
        recurse = catMaybes $ map (\y -> part2' (i - 1) y) possibleSeqs

day17 :: IO (Int, Int)
day17 = do
  input <- readFile "src/y2024/inputs/day17.txt"
  let p1' = parse' input ((part1 0) <$> pComputerState)
  -- turn list output into int
  let p1 = let n = length p1' in sum $ map (\(x, i) -> x * (10 ^ i)) $ zip p1' [n - 1, n - 2 .. 0]
  let p2 = (parse' input (part2 <$> pComputerState))
  pure $ (p1, p2)
