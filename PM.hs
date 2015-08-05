{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (newtheorem, deflike, thmlike, definition, mathpreamble)
import MarXup.Tex
import MarXup.Diagram
import MarXup.LineUp.Haskell
import MarXup.Verbatim
import MarXup.Latex.Math (ensureMath)
import Control.Monad (forM_)

main :: IO ()
main = renderTex SIGPlan "Prettiest" (preamble (header >> mainText))

preamble body = do
  documentClass "../PaperTools/latex/sigplanconf" []
  stdPreamble
  usepackage "tikz" []
  usepackage "polytable" []
  usepackage "url" []
  mathpreamble
  cmd "input" $ tex "../PaperTools/latex/unicodedefs"
  newtheorem "principle" "Principle"

  title "An Insanely Pretty Printer"
  authorinfo [AuthorInfo "Jean-Philippe Bernardy" "bernardy@chalmers.se" "CTH"]
  env "document" body

principle :: TeX -> TeX -> Tex SortedLabel
principle titl = deflike "Principle" "principle" titl


header :: Tex ()
header = do
  -- maketitle
  -- abstract
  keywords $ [ "Pearl", "Pretty Printing"]
  return ()

bibliographyAll :: TeX
bibliographyAll = do
  bibliographystyle "abbrvnat"
  bibliography  "/home/bernardy/repo/PaperTools/bibtex/jp"

mainText :: Tex ()
mainText = «

@intro<-section«Introduction»

@haskell«
  import Data.Function
  import Data.List (intercalate, minimumBy, sort, groupBy)
  import System.Clock
»


A pretty printer is a program that renders data structures in a way which
makes them pleasant to read. (The data structures in question are
often represent programs, but not always.) I propose the following
three laws of pretty printing:

@pcp_visibility<-principle«Visibility»«Pretty printer shall
layout all its output within the width of the page.»

@pcp_compact<-principle«Compactness»«A pretty printer shall minimize the amount of
space used to display the data.»

@pcp_layout<-principle«Layout»« A pretty printer shall make clever use of layout, to make it easy
             for a human to recognise the hierarchical organisation of data.»

The first principle takes precedence over the second one, which itself takes precedence over the third one.


The functional programming community often uses pretty
printing to showcase proper program design: the pretty printer of @citet"hughes_design_1995"
remains an influential example of functional programming design, while
@citet"wadler_prettier_2003" has appeared as chapter of of a book dedicated to the @qu"fun of programming".
Even though Hughes and Wadler are not explicit about it, it is apparent that (some of) the principles stated above
guide the design of their libraries.


In addition of esthetical and pedagogical value, Hughes and Wadler's
provide practical implementations which form the basis of pretty
printing packages which remain popular today:

@descList[(«pretty», url«https://hackage.haskell.org/package/pretty»)
         ,(«wl-print», url«https://hackage.haskell.org/package/wl-pprint»)]

In this paper, I propose a new design for a pretty printing
library. The interface is inspired by Hughes and Wadler's, but is
subtly different. In contrast to Hughes and Wadler, my primary goal is
to abide by the principles of pretty printing as defined above;
efficiency is a secondary concern. (Yet the final result is reasonably
efficient.)  As Hughes and Wadler, I will aim at using a
mathematically-supported methodology and a clean design. Finally,
I will draw general conclusions on how to improve on functional
programming methodologies.

@sec_api<-section«A Pretty API»

We justify our choice of API (set of combinators) by examining a
simple yet typical pretty printing task.  Let us assume we want to
pretty print S-Expressions, and that they are represented as follows:

@haskell«
data SExpr where
   SExpr :: [SExpr] -> SExpr
   Atom :: String -> SExpr
  deriving Show
»
Using the above representation, the S-Expr @teletype«(a b c d)» has the
following encoding:

@haskell«
abcd :: SExpr
abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]
»
Let us specify pretty printing of an S-Expr as follows. In a pretty
display of an S-Expr, we would like the elements to be either
concatenated horizontally, or aligned vertically. The possible pretty
layouts of our example would be either

@verbatim«
(a b c d)
»
or

@verbatim«
(a
 b
 c
 d)
»

The above rules embody .

The goal of the pretty printer is to render a given S-Expr
@itemList[«according to the above rules, which embody @pcp_layout,»
         ,«in as few lines as possible (@pcp_compact) and»
         ,«within the width of a page (@pcp_visibility)»]


Traditionally, a pretty printing library gives us the means to express
the specification of possible pretty layouts: it is up to the user to
reify (@pcp_layout) on the data structure of interest. The printer
will then automatically pick the smallest (@pcp_compact) which fits
the page (@pcp_visibility).

We will not depart from the tradition. Our library will provide an API
to decribe layout which is similar to Hughes's: we can express both
vertical (@hask«$$») and horizontal (@hask«<>») composition of
documents, as well as embedding raw @hask«text» and provide
automatic choice between layouts (@hask«<|>»). At this stage, we keep
the representation of documents abstract using a typeclass, which
provides the above combinators, as well as means of @hask«render»ing a
document:

@haskell«
text    :: Doc d => String -> d
(<>)    :: Doc d => d -> d -> d
($$)    :: Doc d => d -> d -> d
(<|>)   :: Doc d => d -> d -> d
render  :: Doc d => d -> String
»

We can then define a few useful combinators on top of the above: the
@hask«empty» document; concatenation with an intermediate space
@hask«(<+>)»; vertical and horizontal concatenation of multiple
documents.

@haskell«
empty :: Layout d => d
empty = text ""

(<+>) :: Layout d => d -> d -> d
x <+> y = x <> text " " <> y

hsep,vcat :: Doc d => [d] -> d
vcat = foldr1 ($$)
hsep = foldr1 (<+>)
»

We can furthermore define automatic choice between horizontal and
vertical concatenation:

@haskell«
sep :: Doc d => [d] -> d
sep [] = empty
sep xs = hsep xs <|> vcat xs
»

Turning S-expressions into a pretty document is then child's play:

@haskell«
pretty :: Doc d => SExpr -> d
pretty (Atom s) = text s
pretty (SExpr xs) = text "(" <> (sep $ map pretty xs) <> text ")"
»

@sec_semantics<-section«A Pretty Rendering»

We have given a syntax for describing documents, but what should be
its semantics?  What does it mean to pretty print a document? Technically,
what is the specification of @hask«render»?

Let us use an example to try and answer the question. Suppose we want
to pretty print the following S-Expr (which is specially crafted to
demonstrate issues with both Hughes and Wadler libraries):

@haskell«
testData :: SExpr
testData = SExpr [SExpr [Atom "12345", abcd4],
                  SExpr [Atom "12345678", abcd4]]
  where abcd4 = SExpr [abcd,abcd,abcd,abcd]
»
Remember that would like elements inside an S-Expr to be either
aligned vertically or concatenated horizontally (for legibility,
@pcp_layout), The second option will be preferred over the first
(@pcp_compact), as long as the text fits within the page width
(@pcp_visibility).
Thus, on a 80-column-wide page, we would like to get the result displayed in @fig_eighty.

@fig_eighty<-figure_«
Example expression printed on 80 columns. The first line is a helper showing the column of a given character.
»«
@verbatim«
12345678901234567890123456789012345678901234567890123456789012345678901234567890

((12345 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (12345678 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
»»

Printed on a 20-column-wide page, we would like to get the following output:

@verbatim«
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
»
Unfortunately, in this case, and using Hughes' library, we would get the following output instead:

@verbatim«
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
»

The above output uses way more space than
necessary, violating @pcp_compact.  
Why is that? Hughes states that @qu«it would be
unreasonably inefficient for a pretty-printer do decide whether or not
to split the first line of a document on the basis of the content of
the last.» (sec. 7.4 of his paper).  Therefore, he chooses a greedy
algorithm, which tries to fit as much as possible on a single line,
without regard for what comes next.  In our example, the algorithm
fits @teletype«(12345678 ((a», but then it has committed to a very deep
indentation level, which forces to display the remainder of the
document in a narrow space.

One may wonder what would happen with Wadler's library. Unfortunately,
its API cannot even express the layout we are after! Indeed, one can
only specify a @emph«constant» amount of indentation, not one that depends
on the contents of a document.  This means that Wadler's library lacks
the capability to express that a multi-line sub-document @hask«b» should be
laid out to the right of a document @hask«a»: @hask«a» must be put below @hask«b».
Thus, with an appropriate specification, Wadler would render our
example as follows:

@verbatim«
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
»

It's not too bad... but it inserts a spurious line break after the atom
@teletype«12345678». While this may be acceptable to some, I find it
disappointing for two reasons. First, spurious line breaks may appear
in many situations, so the rendering may be much longer than necessary violating @pcp_compact.
Second, and more importantly, the element which is rejected to a next
can only be indented by a constant amount. Let us say we would like to
pretty print the following ml-style snippet:
@verbatim«
Pattern = expression [listElement x,
                      listElement y,
                      listElement z,
                      listElement w]
»
Wadler will force us to lay it out like so:
@verbatim«
Pattern = expression
  [listElement x,
   listElement y,
   listElement z,
   listElement w]
»
Aligning the argument of the expression below the equal sign is bad:
it obscures the structure of the program.
@pcp_layout is not respected. In fact, the lack of a combinator
for alignment proved too constraining for Leijen. Indeed, in his
implemenation of Wadler's design (which is a de-facto standard implementation) he
an alignment combinator. But, also using a greedy algorithm, it
necessarily suffers from the same issues as Hughes library.

Because of the limitations of greediness, I propose to give up on it.
In exchange for its lack of greediness,
the library which I present in the rest of the paper
guarantees to find the smallest output, while 
retaining the ability to express vertical alignment of sub-documents.
Yet, the final algorithm that I will arrive at is fast enough for use
in common pretty printing tasks.

@subsection«A Prettier API»

Before getting into the problem of making a fast (enough)
implementation, I'll refine the API for defining pretty layouts, and give 
formal
semantics for it. 

Recall that we have inherited from Hughes a draft API:

@spec«
text  :: String -> d
(<>)  :: Doc d => d -> d -> d
($$)  :: Doc d => d -> d -> d
»

(I first ignore disjuction between
possible layouts (`<|>`).)

At this stage, classic functional pearls would state a number of laws
that the above API has to satisfy, then infer a semantics from them.
Fortunately, in our case, a straightforward interpretation for document
works, so we can jump straight to the semantics and derive the laws from it.

Let us interpret a layout as a non-empty list of lines to print. I'll
simply use the type of lists and remember on the side the invariant.

@haskell«
type L = [String] -- non empty.
»

@haskell«
instance Layout L where
»
Preparing a layout for printing is as easy as concatenation with
newlines:

@haskell«
  render :: L -> String
  render = intercalate "\n"
»
Embedding a string is thus immediate:

@haskell«
  text :: String -> L
  text s = [s]
»
The interpretation of vertical concatenation ($$) requires barely more
thought:

@haskell«
  ($$) :: L -> L -> L
  xs $$ ys = xs ++ ys
»
What does horizontal concatenation (<>) mean? We will stick to Hughes'
advice: "translate [to the right] the second operand, so that is tabs
against the last character of the first operand". We can represent the
situation diagramatically as follows:

@horizCat

Thus we handle the last line of the first layout and the first line of
the second layout specially, as follows:

@haskell«
  (<>) :: L -> L -> L
  xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
     where xs0 = init xs
           x = last xs
           n = length x
           indent = replicate n blankChar
»
The trained eye will detect that, given the above semantics, vertical
concatenation is (nearly) a special case of horizontal composition. That is,
instead of composing vertically, one can add an empty line (flush) to the
left-hand-side layout and compose horizontally.

TODO: flush ==> flush

@spec«
  ($$) :: L -> L -> L
  a $$ b = flush a <> b
»
where

@haskell«
  flush :: L -> L
  flush xs = xs ++ [""]
»

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

@haskell«
class Layout d where
  (<>) :: d -> d -> d
  text :: String -> d
  flush :: d -> d
  render :: d -> String
»
Additionally, as mentioned above, layouts follow a number of algebraic
laws:

- text is homomorphism for concatenation:

@spec«
prop_text s t = text s <> text t == text (s ++ t)
»

Which justifies the definition we gave previously for the empty document:

@spec«
empty = text ""
»

- Layouts form a monoid with empty and <>

@haskell«
prop_leftUnit :: (Doc a, Eq a) => a -> Bool
prop_leftUnit a = empty <> a == a
»

@haskell«
prop_rightUnit :: (Doc a, Eq a) => a -> Bool
prop_rightUnit a = a <> empty == a
»

@haskell«
prop_assoc :: (Doc a, Eq a) => a -> a -> a -> Bool
prop_assoc a b c = (a <> b) <> c == a <> (b <> c)
»

- flushing can be pushed in concatenation:

@haskell«
prop_flush :: (Doc a, Eq a) => a -> a -> Bool
prop_flush a b = flush (flush a <> b) == flush a <> flush b
»

Note that laws may only @emph«partially» specify the behaviour, while a
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

@haskell«
class Layout d => Doc d where
  (<|>) :: d -> d -> d
»
Documents can be defined as the free commutative monoid of layouts (i.e. a bag of
layouts). The layout operators can be lifted in the natural manner.

We omit the unit of the monoid in the interface. Indeed, it
corresponds to a document with cannot be laid out, which turns out to
be useless as an API for pretty printing.

Thus, we chose as a representation for documents the list of possible
layouts.

@haskell«
newtype F a = F {fromF :: [a]}
  deriving (Functor,Applicative,Show)
»

@haskell«
instance Doc (F L) where
  F xs <|> F ys = F (xs ++ ys)
»

@haskell«
instance Layout (F L) where
  text = pure . text
  flush = fmap flush
  xs <> ys = (<>) <$> xs <*> ys
»

Rendering a document is merely picking the shortest layout among the
valid ones:

@haskell«
  render = render . minimumBy (compare `on` length) . filter valid . fromF
»

-- >   render = render . minimumBy better . fromF

where

@haskell«
better :: L -> L -> Ordering
better a b | not (valid b) = LT
better a b | not (valid a) = GT
better a b = compare (length a) (length b)
»

@haskell«
valid :: L -> Bool
valid xs = maximum (map length xs) <= 80
»

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


TODO: diagram

@haskell«
measure :: L -> M
measure xs = M {maxWidth   = maximum $ map length $ xs,
                height     = length xs - 1,
                lastWidth  = length $ last $ xs}
»

@haskell«
data M = M {height     :: Int,
            lastWidth  :: Int,
            maxWidth   :: Int}
  deriving (Show,Eq,Ord)
»



@haskell«
instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  a <> b = M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
              height = height a + height b,
              lastWidth = lastWidth a + lastWidth b}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  render (M lw h mw) = render $ replicate h (replicate mw 'x') ++ [replicate lw 'x']
»

The equations above are correct if they make `measure` a layout
homomorphism (ignoring of course render):

@spec«
measure (a <> b) = measure a <> measure b
measure (flush a) = flush (measure a)
»

TODO: diagram

@haskell«
fits :: M -> Bool
fits x = maxWidth x <= 80
»
Early filtering out invalid results
-----------------------------------

@spec«
xs <> ys = filter valid [x <> y | x <- xs, y <- ys]
»

This is because width is monotonous:

@spec«
width (a <> b) ≥ width a  and width (a <> b) ≥ width b
width (flush a) ≥ with a
»

and therefore so is validity: keeping invalid layouts is useless: they
can never be combined with another layout to produce something valid.

@spec«
valid (a <> b)  ==> valid a  and  valid b
valid (flush a) ==> valid a
»

Pruning out dominated results
-----------------------------

Idea: filter out the results that are dominated by other results.

@haskell«
class Poset a where
  (≺) :: a -> a -> Bool
»

@spec«
measure a ≺ measure b   =>   a ≤ b
»

Find an instance `Poset M` such that:

if    d1 ≺  d2 and  d'1 ≺  d'2
then  (d1 <> d2) ≺  (d'1 <> d'2) and
      flush d1 ≺ flush d2



@haskell«
instance Poset M where
  M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2
»


@haskell«
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys
»

@haskell«
mergeAll :: Ord a => [[a]] -> [a]
mergeAll [] = []
mergeAll (x:xs) = merge x (mergeAll xs)
»


@haskell«
type D0 = [(M,L)]

instance Layout D0 where
  xs <> ys = bests [ filter (fits . fst) [x <> y | y <- ys] | x <- xs]
  flush xs = bests $ map sort $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = pareto' [] $ sort $ (map flush xs)
  text s = [text s | valid (text s)]
  render (x:_) = render x
»

@haskell«
instance Doc D0 where
  xs <|> ys = bests [xs,ys]
»

@haskell«
bests = pareto' [] . mergeAll
»

@haskell«
pareto' :: Poset a => [a] -> [a] -> [a]
pareto' acc [] = []
pareto' acc (x:xs) = if any (≺ x) acc
                       then pareto' acc xs
                       else x:pareto' (x:acc) xs
»

Because the input is lexicographically sorted, everything which is in
the frontier can't be dominated; hence no need to refilter the
frontier when we find a new element.

(x0,y0,z0) <= (x1,y1,z1)

x0 < x1 or
x0 = x1 and y0 < y0
x0 = x1 and y0 = y0 and z0 < z1

At least one variable is less.

We make sure that concatenation preserve the lexicographic order
(thanks Nick):

if    d1 <=  d2 and  d'1 <=  d'2
then  (d1 <> d2) <=  (d'1 <> d'2)

Flush does not, so we have to re-sort the list.

Hughes-Style nesting
====================

Hughes proposes a nest conbinator.
Mostly used for "hanging":

@haskell«
hang :: Doc d => Int -> d -> d -> d
hang n x y = (x <> y) <|> (x $$ nest n y)
»
His nesting is optional, but in the context of hang, it does not need to be.

@haskell«
nest :: Layout d => Int -> d -> d
nest n y = spaces n <> y
»

Wadler-Style Nesting
====================

Already discussed.

4. Ribbon length

Note that we pick the narrowest result fitting on min. lines lines!

5. Using something better than strings for text

»

lineHeight = 6

abstrLayout :: Expr -> Diagram (Object,Point)
abstrLayout lastWidth = do
  bx <- box
  let points@[_nw,_ne,_se,after,_sse,_sw]
        = [bx # NW
          ,bx # NE
          ,bx # SE + Point 0 lineHeight
          ,bx # SW + Point lastWidth lineHeight
          ,bx # SW + Point lastWidth 0
          ,bx # SW]
      p = polygon points
  path p
  return (Object p bx, after)

lw1 = 12
lw2 = 18

twoLayouts :: Diagram (Object,Point,Object)
twoLayouts = do
  (a,aa) <- draw $ abstrLayout lw1
  (b,_) <- draw $ abstrLayout lw2
  width a === 48
  width b === 56
  height a === 4 *- lineHeight
  height b === 3 *- lineHeight
  return (a,aa,b)

horizCat :: Dia
horizCat = do
  (a,mid,b) <- twoLayouts
  op <- labelObj "<>"
  let lhsObjs = [a,op,b] 
  spread hdist 10 lhsObjs
  align ypart $ map (#Center) $ lhsObjs
  lhs <- boundingBox lhsObjs
  
  eq <- labelObj "="
  
  (a',mid',b') <- twoLayouts
  b' # NW .=. mid'

  lhs # S .=. a' # N + Point 0 20

  -- (cat,_) <- draw $ abstrLayout (lw1 + lw2)

  return () 


spec = haskell

verbatim :: Verbatim () -> TeX
verbatim (Verbatim s _) =
    env "verbatim" (tex s)

-- hask = ensureMath . cmd "mathsf"
hask :: Verbatim a -> Tex ()
hask = ensureMath . haskellInline


url :: TeX -> TeX
url = cmd "url"
