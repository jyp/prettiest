% The *Real* Prettiest Printer
% Jean-Philippe Bernardy

This blog post is a literate Haskell file. Here is the header needed
to compile the file with ghc 7.8.3

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PostfixOperators, ViewPatterns, RecordWildCards, GADTs #-}
> module Blog2 where

> import Data.Function
> import Data.List


Layouts
=======

API
---

-   `empty`: The empty document
-   `(<>)`: horizontal concatenation
-   `($$)`: vertical concatenation
-   `text`: insert a meaningful piece of text
-   `spacing`: insert a non-meaningful text (spaces or typographical marks)
-   `nest`: nest the argument
-   `(<|>)`: disjunction of layouts

> class Layout d where
>   empty :: d
>   (<>) :: d -> d -> d
>   text :: String -> d
>   nest :: Int -> d -> d
>   close :: d -> d
>   render :: d -> String

> ($$) :: Layout d => d -> d -> d
> a $$ b = close a <> b


> (%) :: Doc d => d -> d
> (%) = close

Semantics
---------

Semantics for layouts: list of strings

> type L = [String]

> viewLast :: L -> ([String],String)
> viewLast xs = (init xs, last xs)

> indent :: Int -> String -> String
> indent n x = replicate n ' ' ++ x


> instance Layout L where
>   empty = text ""
>   (viewLast -> (xs,x)) <> (y:ys) = xs ++ [x ++ y] ++ nest (length x) ys
>   close xs = xs ++ [""]
>   text s = [s]
>   nest n = map (indent n)
>   render xs = intercalate "\n" xs

Layouts form a monoid with empty and <>

Nesting accumulates

> prop_nest :: (Doc a, Eq a) => a -> Int -> Int -> Bool
> prop_nest a i j = nest i (nest j a) == nest (i+j) a

> prop_nest0 :: (Doc a, Eq a) => a -> Bool
> prop_nest0 a = nest 0 a == a

closing can be pushed in concatenation:

> prop_close :: (Doc a, Eq a) => a -> a -> Bool
> prop_close a b = (((a%) <> b)%) == (a%) <> (b%)


\begin{spec}
(d1 <> d2) <> d3 == d1 <> (d2 <> d3)

consequence of >>= assoc (list compr), ++ assoc

(d1 <> d2) <> d3
def
 (\i0 -> [(i2,t1 ++ t2) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1]) <> d3
alpha
 (\ia0 -> [(ia2,ta1 ++ ta2) | (ia1,ta1) <- d1 ia0, (ia2,ta2) <- d2 ia1]) <> d3
def
 \i0 -> [(i3,t1 ++ t3) | (i1,t1) <- (\ia0 -> [(ia2,ta1 ++ ta2) | (ia1,ta1) <- d1 ia0, (ia2,ta2) <- d2 ia1]) i0, (i3,t3) <- d3 i1]
beta
 \i0 -> [(i3,t1 ++ t3) | (i1,t1) <- [(ia2,ta1 ++ ta2) | (ia1,ta1) <- d1 i0, (ia2,ta2) <- d2 ia1], (i3,t3) <- d3 i1]
=<< assoc
 \i0 -> [(i3,t1 ++ t3) | (ia1,ta1) <- d1 i0, (ia2,ta2) <- d2 ia1, let i1 = ia2; t1 =ta1++ta2,   (i3,t3) <- d3 i1]
beta
 \i0 -> [(i3,(ta1 ++ ta2) ++ t3) | (i1,ta1) <- d1 i0, (ia2,ta2) <- d2 i1,  (i3,t3) <- d3 ia2]
alpha
\i0 -> [(i3,(t1 ++ t2) ++ t3) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1, (i3,t3) <- d3 i2]
++ assoc
\i0 -> [(i3,t1 ++ t2 ++ t3) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1, (i3,t3) <- d3 i2]

\end{spec}


Precomputing metrics
--------------------

> data M = M {width :: Int,
>             mlines :: Int,
>             colDiff :: Int,
>             mtext :: L}
>   deriving (Show)

> instance Layout M where
>   empty = M 0 0 0 empty
>   text s = M (length s) 0 (length s) (text s)
>   (M w1 h1 c1 s1) <> (M w2 h2 c2 s2) = M (max w1 (w2 + c1)) (h1 + h2) (c1 + c2) (s1 <> s2)
>   close (M w h _ s) = M w (h+1) 0 (close s)
>   nest k (M w h c s) = M (w+k) h (c+k) (nest k s)
>   render (M _ _ _ s) = render s

Documents
=========

Free monoid of <|> (and failure)

> class Layout d => Doc d where
>   (<|>) :: d -> d -> d


> type D0 = [M]

> valid :: M -> Bool
> valid = ((<= 80) . width)


-- > instance Doc D0 where
-- >   empty = [empty]
-- >   xs <> ys = concat [ [x <> y | x <- xs] | y <- ys]
-- >   xs <|> ys = xs ++ ys
-- >   close xs = map close xs
-- >   text s = [text s]
-- >   nest n = map (nest n)
-- >   render = render . minimum . filter valid


Early filtering out invalid results
-----------------------------------

-- >   xs <> ys = [ filter valid [x <> y | x <- xs] | y <- ys]

This is because the width can only ever increase.

Pruning out dominated results
-----------------------------


> measure :: M -> (Int, Int, Int)
> measure (M w h c _) = (h,w,c)

> class Poset a where
>   (≺) :: a -> a -> Bool

> instance Eq M where
>   (==)  = (==) `on` measure
> instance Ord M where
>   compare = compare `on` measure

> instance Poset M where
>   M c1 l1 s1 _ ≺ M c2 l2 s2 _ = c1 <= c2 && l1 <= l2 && s1 <= s2

Then we need a theorem to restrict the computation to pareto frontiers.


Theorem: all operators are monotonous wrt. ≺

if    d1 ≺  d2 and  d'1 ≺  d'2
then  (d1 <> d2) ≺  (d'1 <> d'2) and
      (d1 <|> d2) ≺  (d'1 <|> d'2)
      close d1 ≺ close d2
      nest k d1 ≺ nest k d2


> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge xs [] = xs
> merge (x:xs) (y:ys) = case compare x y of
>   LT -> x:merge xs (y:ys)
>   EQ -> x:y:merge xs ys
>   GT -> y:merge (x:xs) ys
 
> mergeAll :: Ord a => [[a]] -> [a]
> mergeAll [] = []
> mergeAll (x:xs) = merge x (mergeAll xs)

> instance Layout D0 where
>   empty = [empty]
>   xs <> ys = bests $ [ filter valid [x <> y | x <- xs] | y <- ys]
>   close xs = map close xs
>   text s = [text s]
>   nest n = map (nest n)
>   render (x:_) = render x

> instance Doc D0 where
>   xs <|> ys = bests [xs,ys]


> bests :: [[M]] -> [M]
> bests = pareto' [] . mergeAll

> pareto' :: Poset a => [a] -> [a] -> [a]
> pareto' acc [] = acc
> pareto' acc (x:xs) = if any (≺ x) acc
>                        then pareto' acc xs
>                        else pareto' (x:acc) xs

4. Ribbon length

Note that we pick the narrowest result fitting on min. lines lines!

5. Using something better than strings for text















ScratchPad
==========

\begin{spec}
instance Doc D0 where
  empty = \i -> (i,"")
  d1 <> d2 = \i0 -> let (i1,t1) = d1 i0
                        (i2,t2) = d2 i1
                    in (i2,t1 ++ t2) 
  close d = \i -> let (_,t) = d i in (i,t ++ "\n" ++ replicate i ' ')
  text s i = (c,s)
    where c = i + length s
  nest n d = \i -> d (i+n)
  render d = Just $ snd (d 0)


type D1 = Int -> [(Int,String)]

instance Doc D1 where
  empty = \i -> [(i,"")]
  d1 <> d2 = \i0 -> [(i2,t1 ++ t2) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1]
  close d = \i -> [(i,t ++ "\n" ++ replicate i ' ') | (_,t) <- d i]
  text s i = [(c,s) | c < 80]
    where c = i + length s
  d1 <|> d2 = \i -> d1 i ++ d2 i
  nest n d = \i -> d (i+n)
  d1 $$ d2 = close d1 <> d2
  render d = case sortBy (compare `on` fst) (d 0) of
      ((_,s):_) -> Just s
      _ -> Nothing

type D2 = Int -> [(Int,Int,Int,String)] -- height, max col, col, text

instance Doc D2 where
  empty = \i -> [(0,i,i,"")]
  d1 <> d2 = \i0 -> [(h1 + h2,max w1 w2,i2,t1 ++ t2) | (h1,w1,i1,t1) <- d1 i0, (h2,w2,i2,t2) <- d2 i1]
  close d i = [(h+1,w,i,t ++ '\n' : replicate i ' ') | (h,w,_,t) <- d i]
  text s i = [(0,c,c,s) | c < 80]
    where c = i + length s
  d1 <|> d2 = \i -> d1 i ++ d2 i
  nest n d = \i -> d (i+n)
\end{spec}


> x <+> y = x <> text " " <> y
> x </> y = x $$ y
> 
> -- foldSeq k f [] = k
> -- foldSeq k f [x] = x
> -- foldSeq k f xs = foldSeq k f l `f` foldSeq k f r
> --   where (l,r) = splitAt (length xs `div` 2) xs
> 
> foldSeq k f [] = k
> foldSeq k f xs = foldr1 f xs
> 
> 
> sep,hcat,vcat :: Doc a => [a] -> a
> 
> vcat = foldSeq empty ($$)
> hcat = foldSeq empty (<+>)
> 
> sep [] = empty
> sep xs = hcat xs <|> vcat xs
> 
> pretty :: SExpr -> D0
> pretty (Atom s) = text s
> pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"
> 
> abcd = SExpr $ map (Atom . (:[])) "abcd"
> abcd4 = SExpr [abcd,abcd,abcd,abcd]
> testData = SExpr [Atom "axbxcxd", abcd4]
> testData2 = SExpr (replicate 10 testData)
> testData4 = SExpr (replicate 10 testData2)
> testData8 = SExpr (replicate 10 testData4)

> data SExpr where
>   SExpr :: [SExpr] -> SExpr
>   Atom :: String -> SExpr
>  deriving Show
