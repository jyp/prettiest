% The *Real* Prettiest Printer
% Jean-Philippe Bernardy

This blog post is a literate Haskell file. Here is the header needed
to compile the file with ghc 7.10

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PostfixOperators, ViewPatterns, RecordWildCards, GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}
> module Blog2 where

> import Data.Function
> import Data.List


Popular Haskell pretty printers have given me less-than-optimal
results.  This is especially disappointing, as they seem to be the
epitome of functional programs, blessed with the
correct-by-construction methodology of program development.  In this
note I review why I find the current solutions sub-optimal, and propose
a satisfactory alternative.

The state of the art.
=====================

Even today, pretty printing in Haskell is mostly backed by two classic
libraries, either:

1.  The Hughes-Peyton Jones library. The design is [described by
    Hughes](http://belle.sourceforge.net/doc/hughes95design.pdf) in
    *The Design of a Pretty-printing Library*. It has then been
    adopted (and modified) by Peyton Jones, and was distributed with GHC
    for a long time, making it the *de-facto* standard pretty printer.
    It is now available on Hackage in the
    [pretty](https://hackage.haskell.org/package/pretty) package. I believe that
    this remains the dominant design, perhaps disputed by...

2.  The Wadler-Leijen library. In the penultimate chapter of *The Fun
    of Programming*, Wadler re-constructs a pretty printing library
    from scratch. Keeping true to Hughes in particular and the general
    functional programming tradition in general, Wadler starts by
    specifying his library using equational laws, and derives an
    implementation. Leijen took Wadler's implementation and modified it
    to increase its expressivity (but more on that later). The result is
    available in the eponymous
    [wl-pprint](https://hackage.haskell.org/package/wl-pprint)
    package.

Not. Pretty. Enough.
--------------------

Let us assume we want to pretty print S-Expressions:

``` {.example}
data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
```

We'd like to allow to pretty print an S-Expr either horizontally, like
so:

``` {.example}
(a b c d)
```

or vertically, like so:

``` {.example}
(a
 b
 c
 d)
```

The idea is that the pretty printer shoud print the expression in as
few lines as possible.



Taking inspiration from from Hughes, we will allow for both vertical
and horizontal composition. We will also allow for embedding raw text;
and disjunction between layouts.

< text  :: String -> d
< (<>)  :: Doc d => d -> d -> d
< ($$)  :: Doc d => d -> d -> d
< (<|>)  :: Doc d => d -> d -> d

We can then define a few useful combinators on top of the above:


> empty :: Layout d => d
> empty = text ""

> (<+>) :: Layout d => d -> d -> d
> x <+> y = x <> text " " <> y

> sep,hsep,vcat :: Doc d => [d] -> d
> vcat = foldr1 ($$)
> hsep = foldr1 (<+>)


> sep [] = empty
> sep xs = hsep xs <|> vcat xs

> pretty :: SExpr -> D0
> pretty (Atom s) = text s
> pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"

let's suppose we want to pretty print the
following s-expr:

``` {.example}
abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
```

Printed on a wide page, we'd like to get:

``` {.example}
(axbxcxd ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
```

Printed on a narrow page, we'd like to get:

``` {.example}
---------------
(axbxcxd
 ((a b c d)
  (a b c d)
  (a b c d)
  (a b c d)
  (a b c d))
```

NOT This :

``` {.example}
---------------
(axbxcxd ((a
           b
           c
           d)
          (a
           b
           c
           d)
          (a
           b
           c
           d)
          (a
           b
           c
           d)
          (a
           b
           c
           d)))
---------------
```

The thing is, Hughes states that "it would be unreasonably inefficient
for a pretty-printer do decide whether or not to split the first line of
a document on the basis of the content of the last." (sec. 7.4 of his
paper). Therefore, he chooses a greedy algorithm, which tries to fit as
much as possible on a single line, without regard for what comes next.
In our example, the algorithm fits `(axbxcxd ((a`, but then it has
committed to a very deep indentation level, which forces a
less-than-pretty outcome for the remainder of the document.

Wadler's design fares somewhat better. It does not suffer from the above
problem... *by default*. That is, it lacks the capability to express
that sub-documents should be vertically aligned --- compositionally.

(See sec ??? for a discussion)

Layouts
=======


Refinement of the API
---------------------

> spaces :: Layout d => Int -> d
> spaces n = text $ replicate n ' '

> nest :: Layout d => Int -> d -> d
> nest n y = spaces n <> y


Define both a minimal API and its semantics for layouts (without
disjunction).

Interpret a layout as a non-empty list of lines to print.

> type L = [String]

> instance Layout L where

Preparing a layout for printing is as easy as appending a newline
character to each item of the list and concatenate them:

>   render xs = concatMap (++ "\n") xs

The interpretation of embeded strings is thus immediate:

>   text s = [s]

Interpretating the horizontal concatenation requires barely more
thought:

<  xs $$ ys = xs ++ ys

Hughes: "translate [to the right] the second operand, so that is tabs
against the last character of the first operand"

>   xs <> (y:ys) = xs0 ++ [x ++ y] ++ nest (length x) ys
>      where xs0 = init xs
>            x = last xs

The trained eye will detect that, given the above semantics,
horizontal concatenation is nearly a special case of horizontal
composition. That is, instead of composing vertically, one can add an
empty line to the left hand side layout and compose horizontally:

>   close xs = xs ++ [""]

> ($$) :: Layout d => d -> d -> d
> a $$ b = close a <> b

One might argue that this choice of API is not really simpler. Yet,
we will stick with it, for two reasons:

1. The horizontal composition has a nicer algebraic structure, and

2. the efficient implementation is shorter with a single concatenation
operator.


To sum up, our API for layouts is the following:

> class Layout d where
>   (<>) :: d -> d -> d
>   text :: String -> d
>   close :: d -> d
>   render :: d -> String

Algebra
-------

- Layouts form a monoid with empty and <>

> prop_leftUnit :: (Doc a, Eq a) => a -> Bool
> prop_leftUnit a = empty <> a == a

> prop_rightUnit :: (Doc a, Eq a) => a -> Bool
> prop_rightUnit a = a <> empty == a

> prop_assoc :: (Doc a, Eq a) => a -> a -> a -> Bool
> prop_assoc a b c = (a <> b) <> c == a <> (b <> c)

- closing can be pushed in concatenation:

> prop_close :: (Doc a, Eq a) => a -> a -> Bool
> prop_close a b = ((a <> b)%) == a <> (b%)

> (%) :: Doc d => d -> d
> (%) = close

- text is a monoid homomorphism

< prop_text s t = text s <> text t == text (s ++ t)


Precomputing metrics
--------------------

In order to pick the best layout, it will be useful to compute its
metrics. Two metrics are needed: the width (on which there is an upper
bound) and the index of the last line (which we want to minimize).

These metrics are easy to compute:

> height :: L -> Int
> height a = length a - 1

> width' :: L -> Int
> width' = maximum . map length

However, we will not want to recompute these values over and over, so
they should be precomputed; added as meta-data to the layout. We
remark in particular the following properties, which will allow us to
do no recomputation whatsoever of heights:

< height (a <> b) = height a + height b
< height (close a) = 1 + height a

Efficient computation of the width requires to notice that the width
of the last line plays a special role.

> lastW :: L -> Int
> lastW = length . last

Using it we can compute the width efficiently, thanks to the following
equalities:

< width' (a <> b) = max (width a) (lastW a + width b)
< width' (close a) = width' a

< lastW  (a <> b) = lastW a + lastW b
< lastW  (close a) = 0


> data M = M {width :: Int,
>             mlines :: Int,
>             colDiff :: Int,
>             mtext :: L}
>   deriving (Show)

> instance Layout M where
>   text s = M (length s) 0 (length s) (text s)
>   (M w1 h1 c1 s1) <> (M w2 h2 c2 s2) = M (max w1 (w2 + c1)) (h1 + h2) (c1 + c2) (s1 <> s2)
>   close (M w h _ s) = M w (h+1) 0 (close s)
>   render (M _ _ _ s) = render s

Documents
=========

Documents can be defined as the free monoid of layouts (i.e. a list of
layouts). The layout operators can be lifted in the natural manner.

< instance Doc D0 where
<   empty = [empty]
<   xs <> ys = concat [ [x <> y | x <- xs] | y <- ys]
<   xs <|> ys = xs ++ ys
<   close xs = map close xs
<   text s = [text s]
<   nest n = map (nest n)
<   render = render . minimum . filter valid


> class Layout d => Doc d where
>   (<|>) :: d -> d -> d


> type D0 = [M]

> valid :: M -> Bool
> valid = ((<= 80) . width)




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
>   xs <> ys = bests $ [ filter valid [x <> y | x <- xs] | y <- ys]
>   close xs = map close xs
>   text s = [text s]
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

Nesting and hanging
===================

> hang :: Doc d => Int -> d -> d -> d
> hang n x y = (x <> y) <|> (x $$ nest n y)


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


> x </> y = x $$ y
> 
> -- foldSeq k f [] = k
> -- foldSeq k f [x] = x
> -- foldSeq k f xs = foldSeq k f l `f` foldSeq k f r
> --   where (l,r) = splitAt (length xs `div` 2) xs
> 
> 
> 
> 
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
