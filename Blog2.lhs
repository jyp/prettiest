% The *Real* Prettiest Printer
% Jean-Philippe Bernardy

This blog post is a literate Haskell file. Here is the header needed
to compile the file with ghc 7.8.3

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PostfixOperators, ViewPatterns #-}
> module Blog2 where

> import Data.List
> import Data.Function


API
---

Before discussing possible algorithms, we need to chose wisely the the
document-description language that we accept. Daringly standing on
Phil's strong shoulders, I propose the following set of combinators:

-   `empty`: The empty document
-   `(<>)`: horizontal concatenation
-   `($$)`: vertical concatenation
-   `text`: insert a meaningful piece of text
-   `spacing`: insert a non-meaningful text (spaces or typographical marks)
-   `nest`: nest the argument
-   `(<|>)`: disjunction of layouts

> class Doc d where
>   empty :: d
>   (<>) :: d -> d -> d
>   ($$) :: d -> d -> d
>   d1 $$ d2 = close d1 <> d2
>   text :: String -> d
>   nest :: Int -> d -> d
>   (<|>) :: d -> d -> d
>   close :: d -> d
>   render :: d -> Maybe String

> type Di = [String]

> viewLast :: Di -> ([String],String)
> viewLast xs = (init xs, last xs)

> indent n x = replicate n ' ' ++ x

> instance Doc Di where
>   empty = text ""
>   xs $$ ys = xs ++ ys
>   (viewLast -> (xs,x)) <> (y:ys) = xs ++ [x ++ y] ++ nest (length x) ys
>   close xs = xs ++ [""]
>   text s = [s]
>   nest n = map (indent n)
>   render xs = Just $ intercalate "\n" xs

> type D0 = Int -> (Int,String)

> instance Doc D0 where
>   empty = \i -> (i,"")
>   d1 <> d2 = \i0 -> let (i1,t1) = d1 i0
>                         (i2,t2) = d2 i1
>                     in (i2,t1 ++ t2) 
>   close d = \i -> let (_,t) = d i in (i,t ++ "\n" ++ replicate i ' ')
>   text s i = (c,s)
>     where c = i + length s
>   nest n d = \i -> d (i+n)
>   render d = Just $ snd (d 0)

Semantics: function from the current column to a list of possible
outputs. The first component of the tuple is the column reached on the
last line (convenience).

> type D1 = Int -> [(Int,String)]

> instance Doc D1 where
>   empty = \i -> [(i,"")]
>   d1 <> d2 = \i0 -> [(i2,t1 ++ t2) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1]
>   close d = \i -> [(i,t ++ "\n" ++ replicate i ' ') | (_,t) <- d i]
>   text s i = [(c,s) | c < 80]
>     where c = i + length s
>   d1 <|> d2 = \i -> d1 i ++ d2 i
>   nest n d = \i -> d (i+n)
>   d1 $$ d2 = close d1 <> d2
>   render d = case sortBy (compare `on` fst) (d 0) of
>       ((_,s):_) -> Just s
>       _ -> Nothing


> (%) :: Doc d => d -> d
> (%) = close

Documents form a monoid with:
  empty and <>

  failure and <|> (also commutative)

Nesting accumulates

> prop_nest :: (Doc a, Eq a) => a -> Int -> Int -> Bool
> prop_nest a i j = nest i (nest j a) == nest (i+j) a

> prop_nest0 :: (Doc a, Eq a) => a -> Bool
> prop_nest0 a = nest 0 a == a

closing can be pushed in concatenation:

> prop_close :: (Doc a, Eq a) => a -> a -> Bool
> prop_close a b = (((a%) <> b)%) == (a%) <> (b%)


All operators distribute over disjunction

  (a <|> b) <> c == (a <> c) <|> (b <> c)
  etc.

  nest n (d <> e) = nest n d <> nest n e

  nest n (text s) = text s

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

Implementation: two steps.

1. Precomputing shit.

> type D2 = Int -> [(Int,Int,Int,String)] -- height, max col, col, text

> instance Doc D2 where
>   empty = \i -> [(0,i,i,"")]
>   d1 <> d2 = \i0 -> [(h1 + h2,max w1 w2,i2,t1 ++ t2) | (h1,w1,i1,t1) <- d1 i0, (h2,w2,i2,t2) <- d2 i1]
>   close d i = [(h+1,w,i,t ++ '\n' : replicate i ' ') | (h,w,_,t) <- d i]
>   text s i = [(0,c,c,s) | c < 80]
>     where c = i + length s
>   d1 <|> d2 = \i -> d1 i ++ d2 i
>   nest n d = \i -> d (i+n)

2. Pruning out dominated results


4. Ribbon length

Note that we pick the narrowest result fitting on min. lines lines!

3. Using something better than strings for text

