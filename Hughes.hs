{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, DeriveFunctor #-}

module Hughes where

import Data.Monoid
import Data.Function
import Data.List
import Control.Applicative

globalWidth = 80

instance Monoid Doc where
  mempty = nil
  mappend = (.<>)

data Bound x = UnBound | Bound x
  deriving (Eq,Show,Functor)

(.-) :: Bound Int -> Int -> Bound Int
x .- k = (subtract k) <$> x

instance Ord x => Ord (Bound x) where
  UnBound <= _ = True
  _ <= UnBound = False
  Bound x <= Bound y = y <= x

instance Ord x => Monoid (Bound x) where
  UnBound `mappend` x = x
  x `mappend` UnBound = x
  Bound x `mappend` Bound y = Bound (min x y)
  mempty = UnBound

type Condition = Bound Int

feasible :: Condition -> Bool
feasible (Bound col) = col >= 0
feasible _ = True

data M = M {mcond :: Condition,
            mlines :: Int,
            colDiff :: Int,
            mtext :: Int -> (Int,String)}

newtype Doc = Doc {results :: [M]} deriving Show

instance Docu Doc where
  text    s = Doc [M (Bound (globalWidth - length s)) 0 (length s) $ \c -> (c + length s,s)]
  Doc d1 $$ Doc d2 = Doc $ bests $
        [[M (c1 <> c2) (1+n1+n2) s2 $ \c ->
          let (_,x') = t1 c
              (c',x'') = t2 c
          in (c',x'++"\n"++replicate c ' '++x'')
         | M c1 n1 _ t1 <- d1]
         | M c2 n2 s2 t2 <- d2]
  nest k (Doc d) = Doc [ M (c .- k) n (s + k) $ \c -> t (c+n)
                       | M c n s t <- d]
  nil = Doc [M UnBound 0 0 $ \c -> (c,"")]
  Doc d1 .<> Doc d2 = Doc $ bests $
        [[M c' (n1+n2)  (s2 + s1) $ \c -> let (col',x') = t1 c
                                              (c'',x'') = t2 col'
                                          in (c'',x'++x'')
         | M c1 n1 s1 t1 <- d1,let c' = c1 <> (c2 .- s1),  feasible c']
         | M c2 n2 s2 t2 <- d2]
  Doc d1 .<|> Doc d2 = Doc $ bests [d1,d2]

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
mergeBy f (x:xs) (y:ys) = case f x y of
  LT -> x:mergeBy f xs (y:ys)
  EQ -> x:y:mergeBy f xs ys
  GT -> y:mergeBy f (x:xs) ys

mergeAllBy ::(a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy _ [] = []
mergeAllBy f (x:xs) = mergeBy f x (mergeAllBy f xs)

mm :: [[M]] -> [M]
mm = mergeAllBy (compare `on` \M{..} -> (mlines,mcond,colDiff))

filtering :: [M] -> [M]
filtering [] = []
filtering [x] = [x]
filtering (x:y:xs) | mlines x <= mlines y &&
                     mcond x <= mcond y &&
                     colDiff x <= colDiff y = filtering (x:xs)
                   | otherwise = x : filtering (y:xs)

filt :: M -> [M] -> [M]
filt x [] = [x]
filt x (y:xs) | mlines x <= mlines y &&
                mcond x <= mcond y &&
                colDiff x <= colDiff y = filt x xs
              | otherwise = x : filt y xs

filtering' [] = []
filtering' (x:xs) = filt x xs

bests = filtering' . mm

---------------------
-- Debug

instance Show M where
  show (M a b c _) = show (a,b,c)

showBound :: Show a => String -> Bound a -> String
showBound _ UnBound = ""
showBound v (Bound b) = v ++ " <= " ++ show b

------------
-- Examples

example :: Doc
header = text "case x of "
example = header <> body
body = text "abcd" $$ text "efghi"

render1 :: M -> IO ()
render1 x = putStrLn $ snd $ mtext x 0
renderAll :: Doc -> IO ()
renderAll (Doc ms)= mapM_ render1 ms
render :: Doc -> IO ()
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
x <+> y = x .<> text " " .<> y
x </> y = x $$ y

foldSeq k f [] = k
foldSeq k f [x] = x
foldSeq k f xs = foldSeq k f l `f` foldSeq k f r
  where (l,r) = splitAt (length xs `div` 2) xs

-- foldSeq k f [] = k
-- foldSeq k f xs = foldr1 f xs


sep,hcat,vcat :: Docu a => [a] -> a

vcat = foldSeq nil ($$)
hcat = foldSeq nil (<+>)

sep [] = nil
sep xs = hcat xs .<|> vcat xs

pretty :: Docu d => SExpr -> d
pretty (Atom s) = text s
pretty (SExpr xs) = text "(" .<> (sep $ map pretty xs) .<> text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4]
testData2 = SExpr (replicate 10 testData)
testData4 = SExpr (replicate 10 testData2)
testData8 = SExpr (replicate 10 testData4)

class Docu doc where
  (.<>) :: doc -> doc -> doc
  ($$) :: doc -> doc -> doc
  text :: String -> doc
  nest :: Int -> doc -> doc
  (.<|>) :: doc -> doc -> doc
  nil :: doc
spacing s = text s

data Doc0 where
  (:<>) :: Doc0 -> Doc0 -> Doc0
  (:$$) :: Doc0 -> Doc0 -> Doc0
  Text :: String -> Doc0
  Nest :: Int -> Doc0 -> Doc0
  (:<|>) :: Doc0 -> Doc0 -> Doc0
  Nil :: Doc0

instance Docu Doc0 where
  (.<>) = (:<>)
  ($$) = (:$$)
  text = Text
  nest = Nest
  (.<|>) = (:<|>)
  nil = Nil

instance Monoid Doc0 where
  mempty = Nil
  mappend = (:<>)

tst1 :: Docu a => a
tst1 = text "arst" $$ (text "zxcv" .<> text "q")

eval :: Doc0 -> Int -> [(Int,String)]
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

render0 d = putStrLn $ snd $ minimumBy (compare `on` (length . lines . snd)) (eval d 0)
