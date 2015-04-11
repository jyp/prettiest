{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Blog6 where

import Data.Monoid
import Data.Function
import Data.List
import Data.Either
import Control.Applicative
import Data.Array
type Doc = Measure

instance Monoid Measure where
  mempty = nil
  mappend = (.<>)

globalWidth = 80

data M = M {mlines :: Int,
            mrest1 :: Int
            -- mtext :: Int -> Int -> (Int,String)
           } deriving (Show,Eq)
instance Ord M where
  M l1 r1 <= M l2 r2 = l1 <= l2 && r1 >= r2

type Measure = Array (Int,Int) [M]
type Eval = Int -> Int -> [M]

tab ::  Eval -> Measure
tab f = array ((0,0),(40,40)) [((r1,rs),f r1 rs) | r1 <- [0..40], rs <- [0..40]]

d ? (r1,rs) | r1 > 40 = []
            | rs > 40 = []
            | r1 < 0 = []
            | rs < 0 = []
            | otherwise = d!(r1,rs)

text s = tab $ \r1 rs -> [M 0 (r1 - length s) | length s <= r1]
spacing s = tab $ \r1 rs -> [M 0 (r1 - length s)]
align :: Doc -> Doc
align d = tab $ \r1 rs -> d?(r1,r1)
nil = tab $ \r1 rs -> [M 0 r1]
line = tab $ \r1 rs -> [M 1 rs]

d1 .<> d2 = tab $ \r1 rs -> pareto
             [M (n1+n2) r1''
             |M n1 r1' <- d1?(r1,rs),
              M n2 r1'' <- d2?(r1',rs)]

d1 .<|> d2 = tab $ \r1 rs -> pareto $ (d1?(r1,rs)) ++ (d2?(r1,rs))


header = text "case x of"
example = header <> align body
body = text "abcd" <> line <> text "efg"

tst x = x? (40,40)
main :: IO ()
main = print $ (pretty $ testData2)?(40,40)

data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
 deriving Show
x <+> y = x <> spacing " " <> y
x </> y = x <> line <> y

sep :: [Doc] -> Doc
sep [] = mempty
sep xs = foldr1 (<+>) xs .<|> align (foldr1 (</>) xs)
pretty (Atom s) = text s
pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
testData2 = SExpr (replicate 10 testData)


pareto :: Ord a => [a] -> [a]
pareto = pareto' []

pareto' :: Ord a => [a] -> [a] -> [a]
pareto' acc [] = acc
pareto' acc (x:xs) = if any (<= x) acc
                        then pareto' acc xs
                        else pareto' (x:filter (\y -> not (x <= y)) acc) xs
