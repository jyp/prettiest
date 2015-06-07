% An Insanely Pretty Printer
% Jean-Philippe Bernardy

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PostfixOperators, ViewPatterns, RecordWildCards, GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}
> module Blog2 where

> import Data.Function

What is pretty printing.

Used as a test ground for functional programming.

A critical look at design choices of Hughes and Wadler.


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


A Pretty API
------------


Let us assume we want to pretty print S-Expressions, represented as
follows:

``` {.example}
data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
```

Using the above representation, the S-Expr `(a b c d)` is encoded as
follows:

> abcd = SExpr (Atom "a") (Atom "b") (Atom "c") (Atom "d")

In a pretty display of an S-Expr, we would like the elements inside of
an S-Expr to be either concatenated horizontally, or aligned
vertically. The possible pretty layouts of our example would be either

``` {.example}
(a b c d)
```

or

``` {.example}
(a
 b
 c
 d)
```

The goal of a the pretty printer is to print a given s-expression in
as few lines as possible, while respecting the above rules, and
fitting within the width of a page.

A pretty printing library will give us the means to express the
specification of possible pretty layouts, and automatically pick the
prettiest.

Taking inspiration from from Hughes, our library will allow to express
both vertical (`$$`) and horizontal (`<>`) composition of
documents. We will also allow for embedding raw text (`text`) and
automatic choice between layouts (`<|>`). At this stage, we keep the
representation of documents abstract using a typeclass.

< text  :: String -> d
< (<>)  :: Doc d => d -> d -> d
< ($$)  :: Doc d => d -> d -> d
< (<|>)  :: Doc d => d -> d -> d


We can then define a few useful combinators on top of the above: the
empty document; concatenation with an intermediate space; vertical and
horizontal concatenation of multiple documents.

> empty :: Layout d => d
> empty = text ""

> (<+>) :: Layout d => d -> d -> d
> x <+> y = x <> text " " <> y

> hsep,vcat :: Doc d => [d] -> d
> vcat = foldr1 ($$)
> hsep = foldr1 (<+>)

We can then define automatic choice between horizontal and vertical
concatenation:

> sep :: Doc d => [d] -> d
> sep [] = empty
> sep xs = hsep xs <|> vcat xs

Turning S-Expressions into document is then child's play:

> pretty :: Doc d => SExpr -> d
> pretty (Atom s) = text s
> pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"

A Pretty Rendering
------------------

We have given a syntax for describing documents, but what should be
its semantics?  What does it mean to pretty print a document?

Let us use an example to try and answer the question. Suppose we want
to pretty print the following s-expr:

> testData = SExpr [SExpr [Atom "12345", abcd4],
>                   SExpr [Atom "12345678", abcd4]]
>   where abcd4 = SExpr [abcd,abcd,abcd,abcd]

Printed on a wide page, we'd like to get:

``` {.example}
(1234567 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
```

Printed on a 21-column-wide page, we'd like to get:

``` {.example}
---------------
12345678901234567890
((12345 ((a b c d)
         (a b c d)
         (a b c d)
         (a b c d)
         (a b c d)))
 (12345678
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d))))
```

Note that, using Hughes' library, we would get the following
less-than-pretty output:

``` {.example}
12345678901234567890
((12345 ((a b c d)
         (a b c d)
         (a b c d)
         (a b c d)
         (a b c d)))
 (12345678 ((a
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
             d))))
```


Why the long tail? Hughes states that "it would be unreasonably inefficient
for a pretty-printer do decide whether or not to split the first line of
a document on the basis of the content of the last." (sec. 7.4 of his
paper). Therefore, he chooses a greedy algorithm, which tries to fit as
much as possible on a single line, without regard for what comes next.
In our example, the algorithm fits `(1234567 ((a`, but then it has
committed to a very deep indentation level, which forces a
long rendering for the remainder of the document.


One may wonder what would happen with Wadler's API. The answer is that
it cannot even express the layout we are after. Indeed, one can only
specify a *constant* amount of indentation, not one that depends on
the contents of a document.  This means that Wadler's API lacks the
capability to express that a multi-line sub-documents should be laid
out to the right of a document. The best one can hope on our example
is thus:


``` {.example}
123456678901234
((12345
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d)))
 (12345678
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d))))
```


(See sec ??? for a discussion)


A Prettier API
--------------

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

>   xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
>      where xs0 = init xs
>            x = last xs
>            n = length x
>            indent = replicate n ' '

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

It will be useful to compute its metrics: width and the index of the
last line. Indeed, recall that the pretty printer searches for the layout

1. has as few lines as possible and
2. whose width does not exceed a certain size


Given the chosen interpretation of layouts, these metrics are easy to
compute:

> height :: L -> Int
> height = length

> width' :: L -> Int
> width' = maximum . map length

However, we do not want to recompute these values over and over, so
they should be precomputed and tupled with the layout data.  The
traditional approach is to re-cast computation of the attributes as
recursive equations over the API. For the height metric, we have:

< height (a <> b) = height a + height b - 1
< height (text _) = 1
< height (close a) = 1 + height a

< height (xs <> (y:ys)) = length (init xs ++ [last xs ++ y] ++ nest (length x) ys)
<                       = length (init xs)  +  length [last xs ++ y]  + length (nest (length (last xs)) ys)
<                       = length xs - 1     +  1                      + length (map _ ys)
<                       = length xs                                   + length ys
<                       = length xs                                   + length (y:ys) - 1


< width (xs <> (y:ys))
  Assuming x = last xs
           lx = length x
<  = maximum $ map length (init xs ++ [x ++ y] ++ nest lx ys)
<  = maximum $ map length (init xs ++ [x ++ y] ++ nest lx ys)
<  = maximum $ init (map length xs) ++ [length (x ++ y)] ++ map length (nest lx ys)
<  = maximum $ init (map length xs) ++ [length x + length y] ++ map (lx +) (map length ys)
  By monotonicity of maximum
<  = maximum $ (map length xs) ++ [length x + length y] ++ map (lx +) (map length ys)
<  = maximum $ (map length xs) ++  map (lx +) (map length (y:ys))
<  = max (maximum (map length xs)) (maximum (map (lx +) (map length (y:ys))))
<  = max (maximum (map length xs)) (lx + (maximum (map length (y:ys))))
<  = max (width xs)  (lx + width (y:ys))

Thus efficient computation of the width of a layout depends on the
width of the last line of a layout. This new metric can be defined as follows.

> lastW :: L -> Int
> lastW = length . last

It can be efficently computed:

< lastW  (text t) = length t
< lastW  (a <> b) = lastW a + lastW b
< lastW  (close a) = 0

And thus, so can be the width of a layout:

< width' (text t) = length t
< width' (a <> b) = max (width a) (lastW a + width b)
< width' (close a) = width' a

Putting all this reasoning into our implementation, we get:

> data M = M {width :: Int,
>             mlines :: Int,
>             colDiff :: Int,
>             mtext :: L}
>   deriving (Show)

> instance Layout M where
>   text s = M (length s) 0 (length s) (text s)
>   a <> b = M {width = max (width a) (width b + colDiff a),
>               mlines = mlines a + mlines b - 1,
>               colDiff = colDiff a + colDiff b,
>               mtext = mtext a <> mtext b}
>   close (M w h _ s) = M w (h+1) 0 (close s)
>   render (M _ _ _ s) = render s

-- >   (M w1 h1 c1 s1) <> (M w2 h2 c2 s2) = M (max w1 (w2 + c1)) (h1 + h2 - 1) (c1 + c2) (s1 <> s2)

Documents
=========

> class Layout d => Doc d where
>   (<|>) :: d -> d -> d

Documents can be defined as the free monoid of layouts (i.e. a list of
layouts). The layout operators can be lifted in the natural manner.

We omit the unit of the monoid in the interface. Indeed, it
corresponds to a document with cannot be laid out, which turns out to
be useless as an API for pretty printing.

Thus, we chose as a representation for documents the list of possible
layouts;

> type D0 = [M]

< instance Doc D0 where
<   empty = [empty]
<   xs <> ys = concat [ [x <> y | x <- xs] | y <- ys]
<   xs <|> ys = xs ++ ys
<   close xs = map close xs
<   text s = [text s]
<   nest n = map (nest n)

Rendering a document is merely picking the shortest layout among the
valid ones:

<   render = render . minimumBy (comparing length) . filter valid

where

> valid :: M -> Bool
> valid m = width m <= 80




Very inefficient! What can we do about it?



Early filtering out invalid results
-----------------------------------

-- >   xs <> ys = [ filter valid [x <> y | x <- xs] | y <- ys]

This is because width is monotonous:

width (a <> b) > width a  and width (a <> b) > width b

and therefore so is validity: keeping invalid layouts is useless: they
can never be combined with another layout to produce something valid.

valid (a <> b) ==> valid a and valid b


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
>   xs <> ys = bests [ filter valid [x <> y | x <- xs] | y <- ys]
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

Last Width
==========

> data D1 = D1 {minLW :: Int, doc0 :: Int -> D0}
> 
> instance Layout D1 where
>   D1 w1 xs1 <> D1 w2 xs2 = D1 (w1 + w2) (\w ->
>                                           let xs = xs1 w
>                                               ys = xs2 (w - w1)
>                                               val a = width a <= w
>                                           in bests [filter val [x <> y | x <- xs] | y <- ys])
>   text t = D1 (length t) (\w -> [text t | length t <= w])
>   close (D1 _ xs1) = D1 0 (\w -> close (xs1 w))
>   render (D1 _ x) = render (x 80)

-- >   close xs = map close xs
-- >   text s = [text s]
-- >   render (x:_) = render x

> instance Doc D1 where
>   D1 w1 x1 <|> D1 w2 x2 = D1 (min w1 w2) (\w -> x1 w <|> x2 w)

Nesting and hanging
===================

> hang :: Doc d => Int -> d -> d -> d
> hang n x y = (x <> y) <|> (x $$ nest n y)


4. Ribbon length

Note that we pick the narrowest result fitting on min. lines lines!

5. Using something better than strings for text




< minLastW (a <|> b) = min (minLastW a) (minLastW b)
< minLastW (close a) = 0
< minLastW (a <> b) = minLastW a + minLastW b
< minLastW (text t) = length t

< minWidth (a <|> b) = min (minWidth a) (minWidth b)
< minWidth (close a) = minWidth a
< minWidth (a <> b) = minLastW a + minWidth b
< minWidth (text t) = length t









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
