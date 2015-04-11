{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Blog5b where

import Data.Monoid
import Data.Function
import Data.List

instance Monoid Doc where
  mempty = nil
  mappend = (.<>)

globalWidth = 40

data Var = None | Level | CurCol
  deriving (Eq, Show, Ord)
data Expr = Expr Int Var deriving Eq

data Bound x = UnBound | Bound x
  deriving (Eq,Show)

instance Ord x => Ord (Bound x) where
  UnBound <= _ = True
  _ <= UnBound = False
  Bound x <= Bound y = y <= x

instance Ord x => Monoid (Bound x) where
  UnBound `mappend` x = x
  x `mappend` UnBound = x
  Bound x `mappend` Bound y = Bound (min x y)
  mempty = UnBound

cond :: Bound Int -> Bound Int -> Condition
cond x y = Cond (x <> y) y

data Condition = Cond (Bound Int) -- level bound
                      (Bound Int) -- curCol bound
  deriving Eq

instance Ord Condition where
  Cond l1 c1 <= Cond l2 c2 = l1 <= l2 && c1 <= c2

instance Ord Expr where
  Expr k1 v1 <= Expr k2 v2 = k1 <= k2 && v1 <= v2

data M = M {mcond :: Condition,
            mlines :: Int,
            mNewCurCol :: Expr,
            mtext :: Int -> Int -> (Int,String)}

instance Eq M where
  M c1 l1 s1 _ == M c2 l2 s2 _ = c1 == c2 && l1 == l2 && s1 == s2
instance Ord M where
  M c1 l1 s1 _ <= M c2 l2 s2 _ = c1 <= c2 && l1 <= l2 && s1 <= s2

newtype Doc = Doc {fromDoc :: [M]} deriving Show

class Subst x where

(.+) :: Int -> Expr -> Expr
k .+ Expr k' v = Expr (k + k') v

level, curCol :: Expr
level = Expr 0 Level
curCol = Expr 0 CurCol

apply :: Expr -> Condition -> Condition
apply newCurCol (Cond lvl col) = (level <=. lvl) /\ (newCurCol <=. col)

infix 0 .<=.
(.<=.) :: Expr -> Int -> Condition
(.<=.) (Expr k v) n = case v of
  CurCol -> cond UnBound (Bound $ n - k)
  Level -> cond (Bound $ n - k) UnBound
  -- None -> if k < 0 then false else true

(<=.) :: Expr -> Bound Int -> Condition
_ <=. UnBound = true
x <=. Bound y = x .<=. y

(/\) :: Condition -> Condition -> Condition
Cond lvl1 col1 /\ Cond lvl2 col2 = Cond (lvl1 <> lvl2) (col1 <> col2)

after :: Expr -> Expr -> Expr
-- substitute rhs. for curcol in the lhs
Expr k CurCol `after` Expr k' v = Expr (k+k') v
e `after` _ = e

true :: Condition
true = Cond UnBound UnBound
-- false = Cond (Bound (-1))(Bound (-1))

feasible :: Condition -> Bool
feasible (Cond lvl col) = all (>= 0) [x | Bound x <- [lvl,col]]

text    s = Doc [M (newCurCol .<=. globalWidth) 0 newCurCol $ \_ c -> (c + length s,s)]
  where newCurCol = (length s .+ curCol)
spacing s = text s
align (Doc d) = Doc [M (onCol c) n (s' s) (\i c -> t c c) | M c n s t <- d]
  where s' (Expr k v) = Expr k CurCol
        onCol (Cond lvl col) = Cond UnBound (lvl <> col)
nil = Doc [M true 0 curCol $ \_ c -> (c,"")]
line = Doc [M true 1 level $ \i _ -> (i,"\n" ++ replicate i ' ')]
Doc d1 .<> Doc d2 = Doc $ pareto $ concat ms'
  where ms' = [[M c' (n1+n2)  (s2 `after` s1) $ \i c -> let (col',x') = t1 i c
                                                            (c'',x'') = t2 i col'
                                                        in (c'',x'++x'')
               |M c1 n1 s1 t1 <- d1,
                let c' = c1 /\ apply s1 c2,
                feasible c']
              | M c2 n2 s2 t2 <- d2]
Doc d1 .<|> Doc d2 = Doc $ pareto $ d1 ++ d2

pareto :: Ord a => [a] -> [a]
pareto = pareto' []

pareto' :: Ord a => [a] -> [a] -> [a]
pareto' acc [] = acc
pareto' acc (x:xs) = if any (<= x) acc
                        then pareto' acc xs
                        else pareto' (x:filter (\y -> not (x <= y)) acc) xs

---------------------
-- Debug

instance Show M where
  show (M a b c _) = show (a,b,c)

instance Show Expr where
  show (Expr k v) = sm $ filter (not . null) $ [sho v, show k]
   where
    sm [] = "0"
    sm xs = foldr1 op xs
    op x y = case y of
      ('-':y') -> x ++ " - " ++ y'
      _ -> x ++ " + " ++ y
    sho None = ""
    sho Level = "ı"
    sho CurCol = "γ"


showBound :: Show a => String -> Bound a -> [String]
showBound v UnBound = []
showBound v (Bound b) = [v ++ " <= " ++ show b]

instance Show Condition where
   show (Cond UnBound UnBound) = "true"
   show (Cond i d) | i == d = concat $ showBound "γ" d
                   | otherwise = intercalate " ∧ " $  [s | (v,b) <- [("ı",i),("γ",d)], s <- showBound v b]

------------
-- Examples


header = text "case x of"
example = header <> align body
body = text "abcd" <> line <> text "efg"

main :: IO ()
main = do
  -- putStrLn $ snd $ mtext m 0 0
  print $ mms
  where m = minimumBy (compare `on` mlines) mms
        Doc mms = pretty testData4

Doc mms = pretty testData4
    
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
testData4 = SExpr (replicate 10 testData2)

