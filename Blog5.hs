{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Blog5 where

import Data.Monoid
import Data.Function
import Data.List
import Data.Either
import Control.Applicative

type Doc = Measure

instance Monoid Measure where
  mempty = nil
  mappend = (.<>)

type Eval = Int -> Int -> [(String,Int)]

globalWidth :: Expr
globalWidth = 80

data Var = Level | Delta
  deriving (Eq, Show)
data Expr = Expr Int (Var -> Int) -- coefs
instance Show Expr where
  show (Expr k cs) = sm $ filter (not . null)[sho (cs Level) "ι", sho (cs Delta)  "δ", sho k ""]
   where
    sm [] = "0"
    sm xs = foldr1 op xs
    op x y = case y of
      ('-':y') -> x ++ " - " ++ y'
      _ -> x ++ " + " ++ y
    sho 0 _ = ""
    sho 1 x | not (null x) = x
    sho n x = show n ++ x

data C = C Bool Int -- false: Level < x; true : curCol < x


data Bound x = UnBound | Bound x
  deriving (Eq,Show)

instance Ord x => Ord (Bound x) where
  UnBound <= _ = True
  _ <= UnBound = False
  Bound x <= Bound y = y >= x

instance Ord x => Monoid (Bound x) where
  UnBound `mappend` x = x
  x `mappend` UnBound = x
  Bound x `mappend` Bound y = Bound (min x y)
  mempty = UnBound

cond x y = normCond (Cond x y)

normCond :: Condition -> Condition
normCond (Cond x y) = Cond (x <> y) y

data Condition = Cond (Bound Int) -- level bound
                      (Bound Int) -- curCol bound
  deriving Eq

instance Ord Condition where
  Cond l1 c1 <= Cond l2 c2 = l1 <= l2 && c1 <= c2

showBound :: Show a => String -> Bound a -> [String]
showBound v UnBound = []
showBound v (Bound b) = [v ++ " < " ++ show b]

instance Show Condition where
   show (Cond UnBound UnBound) = "true"
   show (Cond i d) | i == d = concat $ showBound "γ" d
                   | otherwise = intercalate " ∧ " $  [s | (v,b) <- [("ı",i),("γ",d)], s <- showBound v b]
data Substitution = Reset Int | Incr Int
  deriving Eq

instance Ord Substitution where
  Reset x <= Reset y = x <= y
  Incr x <= Incr y = x <= y
  Reset x <= Incr y = x <= y

instance Show Substitution where
  show (Reset s) = "δ := ı + " ++ show s
  show (Incr s) = "δ := δ + " ++ show s

data M = M {mcond :: Condition,
            mlines :: Int,
            msubst :: Substitution,
            mtext :: Int -> Int -> (Int,String)}
instance Show M where
  show (M a b c _) = show (a,b,c)
instance Eq M where
  M c1 l1 s1 _ == M c2 l2 s2 _ = c1 == c2 && l1 == l2 && s1 == s2
instance Ord M where
  M c1 l1 s1 _ <= M c2 l2 s2 _ = c1 <= c2 && l1 <= l2 && s1 <= s2

newtype Measure = Measure {fromMeasure :: [M] } deriving Show

instance Num Expr where
  fromInteger x = Expr (fromInteger x) $ \_ -> 0
  Expr k1 cs1 + Expr k2 cs2 = Expr (k1 + k2) (\v -> cs1 v + cs2 v)
  Expr k1 cs1 - Expr k2 cs2 = Expr (k1 - k2) (\v -> cs1 v - cs2 v)
  
var :: Var -> Expr
var x = Expr 0 (\v -> if v == x then 1 else 0)

level :: Expr
level = var Level
delta :: Expr
delta = var Delta

mkCond :: Expr -> Condition
mkCond e@(Expr k cs) = case map cs [Level,Delta] of
  [1,1] -> cond UnBound (Bound $ negate k)
  [1,0] -> cond (Bound $ negate k) UnBound
  [0,0] -> if k < 0 then false else true
  _ -> error $ "condition: " ++ show e

(.<.) :: Expr -> Expr -> Condition
x .<. y = mkCond (x - y)

curCol :: Expr
curCol = level + delta

literal :: Int -> Expr
literal x = Expr x (\_ -> 0)

len :: forall a. [a] -> Expr
len = literal . length

infix 0 .<.

class Subst x where
  apply :: Substitution -> x -> x

(.*) :: Int -> Expr -> Expr
n .* Expr k cs = Expr (n*k) (\v -> n * cs v)

dExpr :: Substitution -> Expr
dExpr (Reset x) = literal x
dExpr (Incr x) = literal x + delta

instance Subst Expr where
  apply s (Expr k cs) = literal k + (cs Level .* level) + (cs Delta .* dExpr s)

instance Subst Condition where
  apply s (Cond lvl col) = (level <. lvl) /\ ((dExpr s + level) <. col)

(<.) :: Expr -> Bound Int -> Condition

_ <. UnBound = true
x <. Bound y = x .<. literal y

(/\) :: Condition -> Condition -> Condition
Cond lvl1 col1 /\ Cond lvl2 col2 = Cond (lvl1 <> lvl2) (col1 <> col2)

after :: Substitution -> Substitution -> Substitution
Reset x `after` _ = Reset x
Incr x `after` Reset y = Reset (x+y)
Incr x `after` Incr y = Incr $ x + y

true :: Condition
true = Cond UnBound UnBound

false = Cond (Bound (-1))(Bound (-1))

class Feasible a where
  feasible :: a -> Bool

instance (Num a, Ord a) => Feasible (Bound a) where
  feasible UnBound = True
  feasible (Bound x) = x >= 0

instance Feasible Condition where
  feasible (Cond lvl col) = feasible lvl && feasible col

merge [] xs = xs
merge xs [] = xs
merge (m@x:xs) (n@y:ys)
  | m < n = x:merge xs (y:ys)
  | m == n = x:y:merge xs ys
  | m > n = y:merge (x:xs) ys

mergeAll [] = []
mergeAll (x:xs) = merge x $ mergeAll xs

best = pareto
-- best = nubBy (\x y -> not (x <= y))
-- best = id

text s = Measure [M (curCol + len s .<. globalWidth) 0 (Incr (length s)) $ \_ c -> (c + length s,s)]
spacing s = Measure [M true 0 (Incr (length s)) $ \_ c -> (c+length s,s)]
align (Measure d) = Measure [M (onCol c) n (s' s) (\i c -> t c c) | M c n s t <- d]
  where s' (Reset k) = Incr k
        s' (Incr k) = Incr k
        onCol (Cond lvl col) = Cond UnBound (lvl <> col)
nil = Measure [M true 0 (Incr 0) $ \_ c -> (c,"")]
line = Measure [M true 1 (Reset 0) $ \i _ -> (i,"\n" ++ replicate i ' ')]
Measure d1 .<> Measure d2 = Measure $ best $ mergeAll ms'
  where ms' = [[M c' (n1+n2)  (s2 `after` s1) $ \i c -> let (col',x') = t1 i c
                                                            (c'',x'') = t2 i col'
                                                        in (c'',x'++x'')
               |M c1 n1 s1 t1 <- d1,
                let c' = c1 /\ apply s1 c2,
                feasible c']
              | M c2 n2 s2 t2 <- d2]

Measure d1 .<|> Measure d2 = Measure $best $ ( d1) ++ ( d2)


header = text "case x of"
example = header <> align body
body = text "abcd" <> line <> text "efg"

main :: IO ()
main = print $  pretty $ testData


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
