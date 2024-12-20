module UTIL.Util (nofail, forceRight) where

nofail :: Maybe a -> a
nofail a = case a of
  Just a' -> a'
  Nothing -> error "fail detected in nofail"

forceRight :: Show b => Either b a -> a
forceRight a = case a of
  Right a' -> a'
  Left e -> error $ show e

