{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables #-}

module Blog5 where

import Data.Monoid
import Data.Function
import Data.List
import Data.Either
import Control.Applicative

data Doc where
  Line :: Doc
  Nil :: Doc
  (:<>) :: Doc -> Doc -> Doc
  Text :: String -> Doc
  Nest :: Int -> Doc -> Doc
  Align :: Doc -> Doc
  (:<|>) :: Doc -> Doc -> Doc
  Spacing :: String -> Doc

instance Monoid Doc where
  mempty = Nil
  mappend = (:<>)

type Eval = Int -> Int -> [(String,Int)]

eval :: Doc -> Eval
eval (Text s) _ c = return (s, c + length s)
eval (Spacing s) i c = return (s, c + length s)
eval Nil i c = return ("",c)
eval (Align d) i c = eval d c c
eval (Nest j d) i c = eval d (min c (i+j)) c
eval Line i c = return ('\n' : replicate i ' ', i)
eval (d1 :<> d2) i c = do
  (t1,c1) <- eval d1 i c
  (t2,c2) <- eval d2 i c1
  return (t1 ++ t2, c2)
eval (d1 :<|> d2) i c = eval d1 i c ++ eval d2 i c

globalWidth :: Expr
globalWidth = 30

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
            msubst :: Substitution}
         deriving (Eq)
instance Show M where
  show (M a b c) = show (a,b,c)
instance Ord M where
  M c1 l1 s1 <= M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2

type Measure = [M]

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
merge (x@m:xs) (y@n:ys) | m < n = x:merge xs (y:ys)
                        | m == n = x:y:merge xs ys
                        | m > n = y:merge (x:xs) ys

uniq :: Eq a => [a] -> [a]
uniq = map head . group

mergeAll [] = []
mergeAll (x:xs) = merge x $ mergeAll xs

best = nubBy (<=)
-- best = nubBy (\x y -> not (x <= y))
-- best = id

measure :: Doc -> Measure
measure (Text s) = [M (curCol + len s .<. globalWidth) 0 (Incr (length s))]
measure (Spacing s) = [M true 0 (Incr (length s))]
measure (Align d) = [M (onCol c) n (s' s) | M c n s <- measure d]
  where s' (Reset k) = Incr k
        s' (Incr k) = Incr k
        onCol (Cond lvl col) = Cond UnBound (lvl <> col)

measure Line = [M true 1 (Reset 0)] where
measure (d1 :<> d2) = best $ mergeAll ms'
  where ms' = [[M c' (n1+n2)  (s2 `after` s1)
               |M c1 n1 s1 <- measure d1,
                let c' = c1 /\ apply s1 c2,
                feasible c']
              | M c2 n2 s2 <- measure d2]

measure (d1 :<|> d2) = best $ (measure d1) ++ (measure d2)


header = Text "case x of"
example = header <> Align body
body = Text "abcd" <> Line <> Text "efg"

main :: IO ()
main = print $ measure $ pretty $ testData


data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
 deriving Show
x <+> y = x <> Spacing " " <> y
x </> y = x <> Line <> y

sep [] = mempty
sep xs = foldr1 (<+>) xs :<|> Align (foldr1 (</>) xs)
pretty (Atom s) = Text s
pretty (SExpr xs) = Text "(" <> (sep $ map pretty xs) <> Text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
testData2 = SExpr (replicate 10 testData)
