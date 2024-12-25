-- BINOMIAL HEAP PRIORITY QUEUE (immutable/pure implementation) 
-- by Alex
{-# LANGUAGE ScopedTypeVariables #-}

module UTIL.PQueue (PQueue, empty, pop, push, merge, extractAll, fromList, pushAll, size, popIdentical) where
import Data.Foldable (minimumBy)
import Data.Function (on)
import Debug.Trace (trace)

-- a Node has a rank, key-value pair and subtrees
data Node k v = Node Int (k, v) [Node k v]
  deriving Show

instance Ord k => Eq (Node k v) where
  a == b = (key a) == (key b)

-- a priority queue is a list of nodes.
type PQueue k v = [Node k v]

key :: Ord k => Node k v -> k
key (Node _ (a, _) _) = a

kv :: Ord k => Node k v -> (k,v)
kv (Node _ tup _) = tup

order :: Ord k => Node k v -> Int
order (Node ord _ _) = ord

children :: Ord k => Node k v -> [Node k v]
children (Node _ (_, _) kids) = kids

empty :: Ord k => PQueue k v
empty = [] 

size :: Ord k => PQueue k v -> Int
size q = sum $ map ((\x -> 2^x) . order) q 

merge :: forall k v. Ord k => PQueue k v -> PQueue k v -> PQueue k v
merge (x:xs) (y:ys)
  | order x > order y = x : (merge xs (y:ys))
  | order x < order y = y : (merge (x:xs) ys)
  | otherwise = tree : (merge xs ys)
  where 
    tree = (mergeTree x y)
    mergeTree :: Node k v -> Node k v -> Node k v
    mergeTree (Node r1 (k1, v1) as) (Node r2 (k2, v2) bs) = 
      if k1 < k2 
      then Node (r1+1) (k1,v1) ((Node r2 (k2, v2) bs):as)
      else Node (r2+1) (k2,v2) ((Node r1 (k1, v1) as):bs)

merge [] [] = []
merge [] ys = ys

merge xs [] = xs

pop :: forall k v. Ord k => PQueue k v -> Maybe ((k,v), PQueue k v)
pop [] = Nothing
pop q = Just (kv target, merge (snip q target) (children target))
  where 
    target = minimumBy (compare `on` key) q

popIdentical :: forall k v. (Show k, Ord k, Eq v, Show v) => PQueue k v -> Maybe ((k,v), PQueue k v)
popIdentical [] = Nothing
popIdentical q = 
  let 
    target = minimumBy (compare `on` key) q
    q' = merge (snip q target) (children target)
  in
    if length q' == 0 || (kv $ minimumBy (compare `on` key) q') /= (kv target)
      then Just (kv target, q')   
      else trace ("popping") popIdentical q'

push :: Ord k => (k,v) -> PQueue k v -> PQueue k v
push x q = merge q [Node 0 x []]


pushAll :: Ord k => [(k,v)] -> PQueue k v -> PQueue k v
pushAll kvs q = foldl' (\q' x -> push x q') q kvs

extractAll :: (Ord k) => PQueue k v -> [(k, v)]
extractAll q = case pop q of
  Nothing -> []
  Just (tup, q') -> tup : extractAll q'

fromList :: Ord k => [(k,v)] -> PQueue k v
fromList kvs = foldl (\q' (k, v) -> push (k,v) q') empty kvs

snip :: Eq a => [a] -> a -> [a]
snip (x:xs) y
  | x == y = xs
  | otherwise = x:(snip xs y)
snip [] _ = []






-- instance Ord k => Ord (Node k v) where
--   a < b = (key a) < (key b)
--   a > b = (key a) > (key b)
--   a >= b = (key a) >= (key b)
--   a <= b = (key a) <= (key b)