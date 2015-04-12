{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Hughes where

import Data.Monoid
import Data.Function
import Data.List

globalWidth = 40

instance Monoid Doc where
  mempty = nil
  mappend = (.<>)

data Var = None | CurCol
  deriving (Eq, Show, Ord)
data Expr = Expr Int Var deriving Eq

(.+) :: Int -> Expr -> Expr
k .+ Expr k' v = Expr (k + k') v

curCol :: Expr
curCol = Expr 0 CurCol

after :: Expr -> Expr -> Expr
-- substitute rhs. for curcol in the lhs
Expr k CurCol `after` Expr k' v = Expr (k+k') v
e `after` _ = e

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

data Condition = Cond (Bound Int) -- level; curCol bound
  deriving Eq

true :: Condition
true = Cond UnBound

feasible :: Condition -> Bool
feasible (Cond (Bound col)) = col >= 0
feasible _ = True

apply :: Expr -> Condition -> Condition
apply newCurCol (Cond col) = (newCurCol <=. col)

infix 0 .<=.
(.<=.) :: Expr -> Int -> Condition
(.<=.) (Expr k v) n = case v of
  CurCol -> Cond (Bound $ n - k)
  None -> if k >= 0 then true else Cond (Bound (-1))

(<=.) :: Expr -> Bound Int -> Condition
_ <=. UnBound = true
x <=. Bound y = x .<=. y

(/\) :: Condition -> Condition -> Condition
Cond col1 /\ Cond col2 = Cond (col1 <> col2)

data M = M {mcond :: Condition,
            mlines :: Int,
            mNewCurCol :: Expr,
            mtext :: Int -> (Int,String)}

newtype Doc = Doc {fromDoc :: [M]} deriving Show

text    s = Doc [M (newCurCol .<=. globalWidth) 0 newCurCol $ \c -> (c + length s,s)]
  where newCurCol = (length s .+ curCol)
spacing s = text s
Doc d1 $$ Doc d2 = Doc $ bests $
      [M (c1 /\ c2) (1+n1+n2) s2 $ \c ->
        let (_,x') = t1 c
            (c',x'') = t2 c
        in (c',x'++"\n"++replicate c ' '++x'')
       | M c2 n2 s2 t2 <- d2, M c1 n1 _ t1 <- d1]
nest :: Int -> Doc -> Doc
nest k (Doc d) = Doc [M (adjust c) n s $ \c -> t (c+n) | M c n s t <- d]
  where adjust (Cond col) = (k .+ curCol <=. col)
nil = Doc [M true 0 curCol $ \c -> (c,"")]
Doc d1 .<> Doc d2 = Doc $ bests $ 
      [M c' (n1+n2)  (s2 `after` s1) $ \c -> let (col',x') = t1 c
                                                 (c'',x'') = t2 col'
                                             in (c'',x'++x'')
       | M c2 n2 s2 t2 <- d2, M c1 n1 s1 t1 <- d1,
         let c' = c1 /\ apply s1 c2,  feasible c']
Doc d1 .<|> Doc d2 = Doc $ bests $ d1 ++ d2

instance Ord Condition where
  Cond c1 <= Cond c2 = c1 <= c2

instance Ord Expr where
  Expr k1 v1 <= Expr k2 v2 = k1 <= k2 && v1 <= v2

instance Eq M where
  M c1 l1 s1 _ == M c2 l2 s2 _ = c1 == c2 && l1 == l2 && s1 == s2
instance Ord M where
  M c1 l1 s1 _ <= M c2 l2 s2 _ = c1 <= c2 && l1 <= l2 && s1 <= s2

-- bests xs = pareto $ [((mlines,(mcond,(mNewCurCol,()))),x) | x@M{..} <- xs]
-- class Pareto a where
--   pareto :: [(a,x)] -> [x]

-- instance Pareto () where
--   pareto xs = map snd xs

-- instance (Ord a, Pareto b) => Pareto (a,b) where
--   pareto xs = concatMap re grps
--     where grps = groupBy ((==) `on` (fst . fst)) $ sortBy (compare `on` (fst . fst)) xs
--           re grp = pareto [(m,x) | ((_,m),x) <- grp]
bests = pareto
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
    sho CurCol = "γ"


showBound :: Show a => String -> Bound a -> String
showBound _ UnBound = ""
showBound v (Bound b) = v ++ " <= " ++ show b

instance Show Condition where
   show (Cond UnBound) = "true"
   show (Cond c) = showBound "γ" c

------------
-- Examples


header = text "case x of "
example = header <> body
body = text "abcd" $$ text "efghi"


render1 x = putStrLn $ snd $ mtext x 0
renderAll (Doc ms)= mapM_ render1 ms
render x = render1 m
  where Doc (m:_) = x
main :: IO ()
main = do
  -- putStrLn $ snd $ mtext m 0 0
  print $ mms
  where Doc mms = pretty testData4

Doc mms = pretty testData4

data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
 deriving Show
x <+> y = x <> text " " <> y
x </> y = x $$ y

sep :: [Doc] -> Doc
sep [] = mempty
sep xs = foldr1 (<+>) xs .<|> (foldr1 (</>) xs)
pretty (Atom s) = text s
pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
testData2 = SExpr (replicate 10 testData)
testData4 = SExpr (replicate 10 testData2)
{-
data Doc where
  (:<>) :: Doc -> Doc -> Doc
  (:$$) :: Doc -> Doc -> Doc
  Text :: String -> Doc
  Nest :: Int -> Doc -> Doc
  (:<|>) :: Doc -> Doc -> Doc
  Nil :: Doc
  
instance Monoid Doc where
  mempty = Nil
  mappend = (:<>)
eval :: Doc -> Int -> [(Int,String)]
eval (Text s) c = [(c',s) | let c' = c+length s, c' <= globalWidth]
eval (Nest n d) c = eval d (c+n)
eval (d1 :<> d2) c = do
  (c',s1) <- eval d1 c
  (c'',s2) <- eval d2 c'
  return (c'',s1++s2)
eval (d1 :$$ d2) c = do
  (_,s1) <- eval d1 c
  (c',s2) <- eval d2 c
  return (c',s1++ "\n"++replicate c ' ' ++s2)
eval (d1 :<|> d2) c = eval d1 c ++ eval d2 c
eval Nil c = [(c,"")]
render d = snd $ minimumBy (compare `on` (length . lines . snd)) (eval d 0)
-}
