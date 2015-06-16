% An Insanely Pretty Printer
% Jean-Philippe Bernardy

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PostfixOperators, ViewPatterns, RecordWildCards, GADTs, NoMonomorphismRestriction, ScopedTypeVariables, InstanceSigs, GeneralizedNewtypeDeriving #-}

> module Paper where
> 
> import Data.Function
> import Data.List (intercalate, minimumBy, sort)

A pretty printing algorithm renders data structures in a way which
makes them pleasant to read. I propose that two principles are
essential to pretty printing:

 1. clever use of layout to make it easy for a human to recognise the
organisation of data

 2. optimisation of the amount of space used

The data structures in question are often program representations,
sometimes generated using meta-programming.


There is a tradition to use pretty printing as a showcase for
functional programming design techniques. Hughes' paper on pretty
printing remains a reference of functional programming design.  Wadler
has built on and simplified Hughes design, and the result appeared as
chapter of of a book dedicated to "beautiful code".  In addition of
esthetical and pedagogical value, Hughes and Wadler's provide
practical implementations which form the basis of pretty printing
packages which remain popular today:

* [pretty](https://hackage.haskell.org/package/pretty)
* [wl-pprint](https://hackage.haskell.org/package/wl-pprint)

In this paper, I propose an alternative design of a pretty printing
library. I'll take a critical look at design choices of Hughes and
Wadler. I will present a library which abides to the principles of
pretty printing as defined above, and which is also reasonably
efficient. Finally I will draw general conclusion on how to improve on
functional programming methodologies.


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

> abcd :: SExpr
> abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]

Let us specify pretty printing of S-Expr as follows. In a pretty
display of an S-Expr, we would like the elements to be either
concatenated horizontally, or aligned vertically. The possible pretty
layouts of our example would be either

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

The goal of the pretty printer is to print a given s-expression in as
few lines as possible, while respecting the above rules, and fitting
within the width of a page.

A pretty printing library will give us the means to express the
specification of possible pretty layouts, and automatically pick the
prettiest.

As Hughes', our library will allow to express both vertical (`$$`) and
horizontal (`<>`) composition of documents, as well as embedding raw
text (`text`) and automatic choice between layouts (`<|>`). At this
stage, we keep the representation of documents abstract using a
typeclass.

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

-- > pretty :: SExpr -> D0

> pretty :: Doc d => SExpr -> d
> pretty (Atom s) = text s
> pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"

A Pretty Rendering
------------------

We have given a syntax for describing documents, but what should be
its semantics?  What does it mean to pretty print a document?

Let us use an example to try and answer the question. Suppose we want
to pretty print the following s-expr (which is specially crafted to
demonstrate issues with both Hughes and Wadler libraries):

> testData :: SExpr
> testData = SExpr [SExpr [Atom "12345", abcd4],
>                   SExpr [Atom "12345678", abcd4]]
>   where abcd4 = SExpr [abcd,abcd,abcd,abcd]

Printed on a 80-column-wide page, we'd like to get:

``` {.example}
12345678901234567890123456789012345678901234567890123456789012345678901234567890
((12345 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (12345678 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
```

Printed on a 20-column-wide page, we'd like to get:

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

Unfortunately Hughes' library, we would get the following output:

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

I find the above result disappointing: it uses way more space than
necessary.  But, why the long tail? Hughes states that "it would be
unreasonably inefficient for a pretty-printer do decide whether or not
to split the first line of a document on the basis of the content of
the last." (sec. 7.4 of his paper).  Therefore, he chooses a greedy
algorithm, which tries to fit as much as possible on a single line,
without regard for what comes next.  In our example, the algorithm
fits `(12345678 ((a`, but then it has committed to a very deep
indentation level, which forces to display the remainder of the
document in a narrow space. My opinion is thus that Hughes fails to
abide by to the second principle of pretty printing: space
optimisation.


One may wonder what would happen with Wadler's libary. Unfortunately,
its API cannot even express the layout we are after! Indeed, one can
only specify a *constant* amount of indentation, not one that depends
on the contents of a document.  This means that Wadler's library lacks
the capability to express that a multi-line sub-document $b$ should be
laid out to the right of a document $a$: $a$ must be put below $b$.
Thus, with an appropriate specification, Wadler would render our
example as follows:

``` {.example}
12345678901234567890
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

It's not too bad! But it inserts a spurious line break after the token
12345678. While this may be acceptable to some, I find it
disappointing for two reasons. First, spurious line breaks may appear
in many situations. Second, the element which is rejected to a next
can only be indented by a constant amount. Let us say we would like to
pretty print the following ml-style snippet:

Pattern = expression [listElement x,
                      listElement y,
                      listElement z,
                      listElement w]

Wadler will force us to lay it out like so:

Pattern = expression
  [listElement x,
   listElement y,
   listElement z,
   listElement w]

Aligning the argument of the expression below the equal sign is bad:
it obscures the structure of the program. The first principle of
pretty printing is not respected. In fact, the lack of a combinator
for alignment proved too constraining for Leijen. Indeed, in his
implemenation of Wadler's design (which is a de-facto standard) he,
added such a combinator. But, also using a greedy algorithm, it
necessarily suffers from the same issues as Hughes library.

Because of the limitations of greediness, I will give up on it. I will
retain the ability to express vertical alignment of sub-documents,
minimize space usage and make a the algorithm fast enough to be usable
in practice.


A Prettier API
--------------

Before getting into the business of making a fast (enough)
implementation, I'll refine the API for pretty layouts, and give a
semantics for it. At this stage, we ignore disjuction (`<|>`) between
possible layouts.

Let us start with a methological warning. I recommend that designing
an API and a semantics should be done as a single creative
act. Indeed, without any underlying meaning, it is difficult to choose
a sensible API. At the same time, building a model can be done only if
one has a rough sketch of how it should be used.

Basing a library on the API is sometimes called a "deep embedding",
and the semantics a "shallow embedding".

The process of gradually refining API and semantics is difficult to
present in a coherent way. Fortunately, in our case, a straightforward
semantics fits the bill.

Recall that we have inherited from Hughes a draft API:

< text  :: String -> d
< (<>)  :: Doc d => d -> d -> d
< ($$)  :: Doc d => d -> d -> d

Let us interpret a layout as a non-empty list of lines to print. I'll
simply use the type of lists and remember on the side the invariant.

> type L = [String] -- non empty.



> instance Layout L where

Preparing a layout for printing is as easy as concatenation with
newlines:

>   render :: L -> String
>   render = intercalate "\n"

Embedding a string is thus immediate:

>   text :: String -> L
>   text s = [s]

The interpretation of vertical concatenation ($$) requires barely more
thought:

<   ($$) :: L -> L -> L
<   xs $$ ys = xs ++ ys

What does horizontal concatenation (<>) mean? We will stick to Hughes'
advice: "translate [to the right] the second operand, so that is tabs
against the last character of the first operand". We can represent the
situation diagramatically as follows:

````
                             bbbbbbbbbbbbbbbb
aaaaaaaaaaaaaaaaaaa          bbbbbbbbbbbbbbbb
aaaaaaaaaaaaaaaaaaa    <>    bbbbbbbbbbbbbbbb
aaaaaaaaaa                   bbbbbb

=

aaaaaaaaaaaaaaaaaaa
aaaaaaaaaaaaaaaaaaa
aaaaaaaaaabbbbbbbbbbbbbbbb
          bbbbbbbbbbbbbbbb
          bbbbbbbbbbbbbbbb
          bbbbbb
````

Thus we handle the last line of the first layout and the first line of
the second layout specially, as follows:

>   (<>) :: L -> L -> L
>   xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
>      where xs0 = init xs
>            x = last xs
>            n = length x
>            indent = replicate n ' '

The trained eye will detect that, given the above semantics, vertical
concatenation is (nearly) a special case of horizontal composition. That is,
instead of composing vertically, one can add an empty line (flush) to the
left-hand-side layout and compose horizontally.

TODO: flush ==> flush

<   ($$) :: L -> L -> L
<   a $$ b = flush a <> b

where

>   flush :: L -> L
>   flush xs = xs ++ [""]


One might argue that replacing ($$) by `flush` does not make the API
shorter, and maybe not even simpler. Yet, we will stick this choice,
for two reasons:

1. The new API clearly separates the concerns of concatenation and
left-flushing documents.

2. The horizontal composition (<>) has a nicer algebraic structure
than ($$). The vertical composition ($$) has no unit, while (<>) forms
a monoid with the empty layout. (Due to a more complicated semantics,
Hughes operator (<>) does not form a monoid.)

To sum up, our API for layouts is the following:

> class Layout d where
>   (<>) :: d -> d -> d
>   text :: String -> d
>   flush :: d -> d
>   render :: d -> String

Additionally, as mentioned above, layouts follow a number of algebraic
laws:

- text is homomorphism for concatenation:

< prop_text s t = text s <> text t == text (s ++ t)

Which justifies the definition we gave previously for the empty document:

< empty = text ""

- Layouts form a monoid with empty and <>

> prop_leftUnit :: (Doc a, Eq a) => a -> Bool
> prop_leftUnit a = empty <> a == a

> prop_rightUnit :: (Doc a, Eq a) => a -> Bool
> prop_rightUnit a = a <> empty == a

> prop_assoc :: (Doc a, Eq a) => a -> a -> a -> Bool
> prop_assoc a b c = (a <> b) <> c == a <> (b <> c)

- flushing can be pushed in concatenation:

> prop_flush :: (Doc a, Eq a) => a -> a -> Bool
> prop_flush a b = flush (flush a <> b) == flush a <> flush b


Note that laws may only *partially* specify the behaviour, while a
semantic model will always fully constrain it.

(exercise: is the above set of laws fully constraining the semantic model?)

Notice that Hughes and Wadler give the semantics via laws first and
come up with a compositional interpretation second. This is fine,
precisely because laws do not fully constrain the design; there is
room for wiggle. However, a compositional semantics is often an even
better guide which should not be an afterthought.



Documents
=========

Proceed to extend the API with choice between layouts and specify the
problem formally.

> class Layout d => Doc d where
>   (<|>) :: d -> d -> d

Documents can be defined as the free monoid of layouts (i.e. a list of
layouts). The layout operators can be lifted in the natural manner.

We omit the unit of the monoid in the interface. Indeed, it
corresponds to a document with cannot be laid out, which turns out to
be useless as an API for pretty printing.

Thus, we chose as a representation for documents the list of possible
layouts.

> newtype F a = F {fromF :: [a]}
>   deriving (Functor,Applicative,Show)

> instance Doc (F L) where
>   F xs <|> F ys = F (xs ++ ys)

> instance Layout (F L) where
>   text = pure . text
>   flush = fmap flush
>   xs <> ys = (<>) <$> xs <*> ys

Rendering a document is merely picking the shortest layout among the
valid ones:

>   render = render . minimumBy (compare `on` length) . filter valid . fromF

-- >   render = render . minimumBy better . fromF

where

> better :: L -> L -> Ordering
> better a b | not (valid b) = LT
> better a b | not (valid a) = GT
> better a b = compare (length a) (length b)

> valid :: L -> Bool
> valid xs = maximum (map length xs) <= 80


An alternative semantics
------------------------

At this point, a classic functional pearl would derive an
implementation via a series of calculational steps. While this may
very well be done, I will instead proceed to give insight into how I
actually designed the library.

Let us remember that we want to select the layout with minimal use of
space. Hence, from an algorithm point of view, all that matters is the
space that a layout takes. Let us define an abstract semantics for
documents which focuses on such space.

This semantics can be guessed by looking at the diagram for
composition of layouts. All that matters is the maximum width of the
layout, the width of its last line and its height (and because layouts
can't be empty we will start counting from 0).


        max width
    <-------------->
    bbbbbbbbbbbbbbbb  ^
    bbbbbbbbbbbbbbbb  |  height
    bbbbbbbbbbbbbbbb  v
    bbbbbbbbbbbb
    <---------->
     last width

> measure :: L -> M
> measure xs = M {maxWidth = maximum $ map length $ xs,
>                 height = length xs - 1,
>                 lastWidth = length $ last $ xs}

> data M = M {lastWidth :: Int,
>             height    :: Int,
>             maxWidth  :: Int}
>   deriving (Show,Eq,Ord)




> instance Layout M where
>   text s = M {height = 0, maxWidth = length s, lastWidth = length s}
>   a <> b = M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
>               height = height a + height b,
>               lastWidth = lastWidth a + lastWidth b}
>   flush a = M {maxWidth = maxWidth a,
>                height = height a + 1,
>                lastWidth = 0}
>   render (M lw h mw) = render $ replicate h (replicate mw 'x') ++ [replicate lw 'x']

The equations above are correct if they make `measure` a layout
homomorphism (ignoring of course render):

< measure (a <> b) = measure a <> measure b
< measure (flush a) = flush (measure a)

         max mw1 (lw2 + w2)
    <----------------------->
         mw1
    <------------>
    aaaaaaaaaaaaaa             ^       ^
    aaaaaaaaaaaaaa             | h1    |
    aaaaaaaaaaaaaa             |       |
    aaaaaaaaaaaaaa             v       | h1 + h2
    aaaaaaaaabbbbbbbbbbbbbbbb  ^       |
    <------->bbbbbbbbbbbbbbbb  |  h2   |
       lw1   bbbbbbbbbbbbbbbb  v       v
             bbbbbbbbbbbb
             <-------------->
                   mw2
             <---------->
                 lw2
   <-------------------->
        lw1 + lw2

> fits :: M -> Bool
> fits x = maxWidth x <= 80

Early filtering out invalid results
-----------------------------------

<   xs <> ys = filter valid [x <> y | x <- xs, y <- ys]

This is because width is monotonous:

width (a <> b) ≥ width a  and width (a <> b) ≥ width b
width (flush a) ≥ with a

and therefore so is validity: keeping invalid layouts is useless: they
can never be combined with another layout to produce something valid.

valid (a <> b)  ==> valid a  and  valid b
valid (flush a) ==> valid a


Pruning out dominated results
-----------------------------

Idea: filter out the results that are dominated by other results.

> class Poset a where
>   (≺) :: a -> a -> Bool

measure a ≺ measure b   =>   a ≤ b

Find an instance `Poset M` such that:

if    d1 ≺  d2 and  d'1 ≺  d'2
then  (d1 <> d2) ≺  (d'1 <> d'2) and
      flush d1 ≺ flush d2



> instance Poset M where
>   M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2



> merge :: Ord a => [a] -> [a] -> [a]
> merge [] xs = xs
> merge xs [] = xs
> merge (x:xs) (y:ys)
>   | x <= y = x:merge xs (y:ys)
>   | otherwise = y:merge (x:xs) ys
 
> mergeAll :: Ord a => [[a]] -> [a]
> mergeAll [] = []
> mergeAll (x:xs) = merge x (mergeAll xs)

> type D0 = [(M,L)]
>
> instance Layout D0 where
>   xs <> ys = bests [ filter (fits . fst) [x <> y | y <- ys] | x <- xs]
>   flush xs = pareto' [] (sort (map flush xs))
>   -- TODO: is sort needed?
>   text s = [text s | valid (text s)]
>   render (x:_) = render x

> instance Doc D0 where
>   xs <|> ys = bests [xs,ys]


> bests = pareto' [] . mergeAll

> pareto' :: Poset a => [a] -> [a] -> [a]
> pareto' acc [] = reverse acc
> pareto' acc (x:xs) = if any (≺ x) acc
>                        then pareto' acc xs
>                        else pareto' (x:acc) xs
> --                        else pareto' (x:filter (not . (x ≺)) acc) xs

Because the input is lexicographically sorted, everything which is in
the frontier can't be dominated; hence no need to refilter the
frontier when we find a new element.

(x0,y0,z0) <= (x1,y1,z1)

x0 < x1 or
x0 = x1 and y0 < y0
x0 = x1 and y0 = y0 and z0 < z1

At least one variable is less.

Min Width and min last width
============================


aaaaaaaaaaaaaaaaaaa
aaaaaaaaaabbbbbbbbbbbbbbbb
          bbbbbb


> data D1 = D1 {minW :: Int, -- min width
>               minLW :: Int, -- min last width
>               doc0 :: Int -> -- available width
>                       Int -> -- available last width (invar: less than the above.)
>                       D0}
>
> instance Layout D1 where
>   D1 m1 w1 xs1 <> D1 m2 w2 xs2 = D1 (max m1 (w1 + m2))
>                                     (w1 + w2)
>                                     (\w lw ->
>                                           let xs = xs1 w (w - m2)
>                                               ys = xs2 (w - w1) (min lw (w - w1))
>                                               val (a,_) = (maxWidth a <= w) && (lastWidth a <= lw)
>                                           in bests [filter val [x <> y | y <- ys] | x <- xs])
>   text t = D1 (length t) (length t) (\w lw -> [text t | length t <= w])
>   flush (D1 m _ xs1) = D1 m 0 (\w lw -> flush (xs1 w w))
>   render (D1 _ _ x) = render (x 80 80)

> instance Metric D1 where
>   meter  (D1 _ _ x) = meter (x 80 80)

-- >   flush xs = map flush xs
-- >   text s = [text s]
-- >   render (x:_) = render x

> instance Doc D1 where
>   D1 m1 w1 x1 <|> D1 m2 w2 x2 = D1 (min m1 m2) (min w1 w2) (\w lw -> x1 w lw <|> x2 w lw)

> data D2 = (:<>) D2 D2 | Text String | Flush D2 |  D2 :<|> D2
>   deriving Eq

> instance Layout D2 where
>   text = Text
>   flush = Flush
>   -- (a :<> b) <> c = a :<> (b <> c)
>   a <> b = a :<> b
>   render = render . fold

> instance Metric D2 where
>   meter = meter . fold

> fold :: D2 -> D1
> fold (Text s) = text s
> fold (Flush a) = flush (fold a)
> fold (a :<> b) = fold a <> fold b
> fold (a :<|> b) = fold a <|> fold b

> instance Doc D2 where
>   (a :<> b) <|> (a' :<> b') | a == a' = a <> (b <|> b')
>   a <|> a' = a :<|> a'

Hughes-Style nesting
====================

Hughes proposes a nest conbinator.
Mostly used for "hanging":

> hang :: Doc d => Int -> d -> d -> d
> hang n x y = (x <> y) <|> (x $$ nest n y)

His nesting is optional, but in the context of hang, it does not need to be.

> nest :: Layout d => Int -> d -> d
> nest n y = spaces n <> y


Wadler-Style Nesting
====================


> data M2 = M2 {heigh :: Int, -- minimize
>               width1 :: Int, -- minimize
>               hasReset :: Bool, -- Better if True.
>               width2 :: Int, -- Note: this does not need to be minimized, just < 80
>               lW2 :: Int, -- minimize
>               l1, l2 :: L}

aaaaaaaaaa               cccccccccccc
aaaaa                    ccccc
-----------------   <>   -------------
bbbbbbbbbbbbbbbb         ddddddd
bbbbbb                   ddd

=

aaaaaaaaaa
aaaaa
-------------------
bbbbbbbbbbbbbbbb
bbbbbbcccccccccccc
      ccccc
ddddddd
ddd

> instance Layout M2 where
>   M2 y w1 True w2 lw a b <> M2 z x1 r x2 lx c d = M2 (z+y) w1 True (maximum [w2,lw+x1,x2]) (if r then lx else lw + lx) a ((b <> c) ++ d)


aaaaaaaaaa               cccccccccccc
aaaaa                    ccccc
                    <>   -------------
                         ddddddd
                         ddd


aaaaaaaaaa
aaaaacccccccccccc
     ccccc
-----------------
ddddddd
ddd


>   M2 y w1 False _ lw a _ <> M2 z x1 r x2 lx c d = M2 (z+y) (max w1 (lw + x1)) r x2 (if r then lx else lw + lx) (a <> c) d
>   text s = M2 0 (length s) False 0 (length s) [s] []
>   flush (M2 x w1 r w2 0 a b) = M2 (x+1) w1 r w2 0 (if r then a else flush a) (if r then flush b else b)
>   flush d = tabulate (d <> line)
>   render (M2 _ _ _ _ _ a b) = render (a ++ b)


> spaces :: Layout d => Int -> d
> spaces n = text $ replicate n ' '

> tab n = M2 1 0 True n n [] [replicate n ' ']

-- > nest n d = tab n <> d

> tabulate (M2 h w1 r w2 lw a b) = M2 h (max w1 w2) False 0 lw (a ++ b) []

> line = M2 1 0 True 0 0 [] [[]]

-- > hang n x y = (x <> y) <|> (x <> nest n y)


4. Ribbon length

Note that we pick the narrowest result fitting on min. lines lines!

5. Using something better than strings for text




< minLastW (a <|> b) = min (minLastW a) (minLastW b)
< minLastW (flush a) = 0
< minLastW (a <> b) = minLastW a + minLastW b
< minLastW (text t) = length t

< minWidth (a <|> b) = min (minWidth a) (minWidth b)
< minWidth (flush a) = minWidth a
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
  flush d = \i -> let (_,t) = d i in (i,t ++ "\n" ++ replicate i ' ')
  text s i = (c,s)
    where c = i + length s
  nest n d = \i -> d (i+n)
  render d = Just $ snd (d 0)


type D1 = Int -> [(Int,String)]

instance Doc D1 where
  empty = \i -> [(i,"")]
  d1 <> d2 = \i0 -> [(i2,t1 ++ t2) | (i1,t1) <- d1 i0, (i2,t2) <- d2 i1]
  flush d = \i -> [(i,t ++ "\n" ++ replicate i ' ') | (_,t) <- d i]
  text s i = [(c,s) | c < 80]
    where c = i + length s
  d1 <|> d2 = \i -> d1 i ++ d2 i
  nest n d = \i -> d (i+n)
  d1 $$ d2 = flush d1 <> d2
  render d = case sortBy (compare `on` fst) (d 0) of
      ((_,s):_) -> Just s
      _ -> Nothing

type D2 = Int -> [(Int,Int,Int,String)] -- height, max col, col, text

instance Doc D2 where
  empty = \i -> [(0,i,i,"")]
  d1 <> d2 = \i0 -> [(h1 + h2,max w1 w2,i2,t1 ++ t2) | (h1,w1,i1,t1) <- d1 i0, (h2,w2,i2,t2) <- d2 i1]
  flush d i = [(h+1,w,i,t ++ '\n' : replicate i ' ') | (h,w,_,t) <- d i]
  text s i = [(0,c,c,s) | c < 80]
    where c = i + length s
  d1 <|> d2 = \i -> d1 i ++ d2 i
  nest n d = \i -> d (i+n)
\end{spec}


> x </> y = x $$ y

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


Why does the semi-greedy version does not work?
===============================================

- At line 2:
  - We have printed more tokens.
  - Current column is less.


1 2 3 4
  5 6 7
      8
      9
      0
      a

But this layout turns out to be shorter:

1 2 3
    4 5
    6 7
    8 9
    0 a



> main :: IO ()
> main = do
>   -- putStrLn $ render $ mms
>   print $ meter $ mms
>   where mms :: D0
>         mms = pretty testData8
>           -- = [mlines | M {..} <- doc0 (pretty testData4) 80 80]



That's right! For example:

aaaaaaa bbbbbbbbbb
        x
        ...
        ...
        ...
        x
        ccccccccc d
                  d
                  d
                  d

aaaaaaa
  bbbbbbbbbb
  x
  ...
  ...
  ...
  x
  ccccccccc d d d d

> ($$) :: Layout d => d -> d -> d
> a $$ b = flush a <> b


> class Metric m where
>   meter :: m -> M

> instance Metric M where
>   meter = id

However, we do not want to recompute these values over and over, so
they should be precomputed and tupled with the layout data.  The
traditional approach is to re-cast computation of the attributes as
recursive equations over the API. For the height metric, we have:

< height (a <> b) = height a + height b - 1
< height (text _) = 1
< height (flush a) = 1 + height a

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
< lastW  (flush a) = 0

And thus, so can be the width of a layout:

< width' (text t) = length t
< width' (a <> b) = max (width a) (lastW a + width b)
< width' (flush a) = width' a

Putting all this reasoning into our implementation, we get:


-- >   (M w1 h1 c1 s1) <> (M w2 h2 c2 s2) = M (max w1 (w2 + c1)) (h1 + h2 - 1) (c1 + c2) (s1 <> s2)

> instance Metric L where
>   meter = measure

> instance Metric D0 where
>   meter  (x:_) = meter x

> instance Metric a => Metric (a,b) where
>   meter = meter . fst

> instance (Layout a, Layout b) => Layout (a,b) where
>   text s = (text s, text s)
>   flush (a,b) = (flush a, flush b)
>   (a,b) <> (c,d) = (a<>c ,b<>d)
>   render = render . snd

> instance (Doc a, Doc b) => Doc (a,b) where
>   (a,b) <|> (c,d) = (a<|>c,b<|>d)

> instance (Poset a) => Poset (a,b) where
>   (a,_) ≺ (b,_) = a ≺ b

