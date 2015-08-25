{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (deflike, thmlike, definition, mathpreamble,lemma)
import MarXup.Tex
import MarXup.Diagram
import MarXup.LineUp.Haskell
import MarXup.Verbatim
import MarXup.Latex.Math (ensureMath)
import Control.Monad (forM_,when)
import Control.Lens (set)

main :: IO ()
main = renderTex SIGPlan "Prettiest" (preamble (header >> mainText >> bibliographyAll))

preamble body = do
  documentClass "../PaperTools/latex/sigplanconf" ["preprint"]
  stdPreamble
  mathpreamble
  cmd "input" $ tex "../PaperTools/latex/unicodedefs"

  title "An Insanely Pretty Printer"
  authorinfo [AuthorInfo "Jean-Philippe Bernardy" "bernardy@chalmers.se" "CTH"]
  env "document" body

principle :: TeX -> TeX -> Tex TeX
principle titl body = do
  deflike "Principle" "principle" "Principle" (smallcaps titl) body
  return $ smallcaps titl


header :: Tex ()
header = do
  maketitle
  -- abstract
  keywords $ [ "Pearl", "Pretty Printing"]
  return ()

bibliographyAll :: TeX
bibliographyAll = do
  bibliographystyle "abbrvnat"
  bibliography  "../PaperTools/bibtex/jp"

haskellPreamble :: Tex ()
haskellPreamble = «
@haskell«
  import Data.Function
  import Data.List (intercalate, minimumBy, sort, groupBy)
  import System.Clock
»»

mainText :: Tex ()
mainText = «

@sec_intro<-section«Introduction»



A pretty printer is a program that renders data structures in a way which
makes them pleasant to read. (The data structures in question
often represent programs, but not always.) I propose the following
three principles of pretty printing:

@pcp_visibility<-principle«Visibility»«A pretty printer shall
layout all its output within the width of the page.»

@pcp_compact<-principle«Compactness»«A pretty printer shall minimize the amount of
space used to display the data.»

@pcp_layout<-principle«Layout»« A pretty printer shall make clever use of layout, to make it easy
             for a human to recognise the hierarchical organisation of data.»

Furthermore, the first principle takes precedence over the second one, which itself takes precedence over the third one.


The functional programming community has been using
pretty printing to showcase proper functional programming style. The pretty printer of @citet"hughes_design_1995"
remains an influential example of functional programming design, while that of
@citet"wadler_prettier_2003" has appeared as chapter of of a book dedicated to the @qu"fun of programming".
Even though Hughes and Wadler are not explicit about it, it is apparent that (some of) the principles stated above
guide the design of their libraries.


In addition to their esthetical and pedagogical value, Hughes and Wadler
provide practical implementations which form the basis of pretty
printing packages, which remain popular today. Hughes' design has been
refined by Peyton Jones, and is available as the
Hackage package@sans«pretty»@footnote«@url«https://hackage.haskell.org/package/pretty»»,
while Wadler's design has been extended by Leijen and made available as the 
@sans«wl-print» package@footnote«@url«https://hackage.haskell.org/package/wl-pprint»».

In this paper, I propose a new design for a pretty printing
library. The interface is inspired by Hughes' and Wadler's, but is
subtly different. In contrast to Hughes and Wadler, my primary goal is
to abide by the principles of pretty printing as defined above;
efficiency is a secondary concern. Yet the final result is reasonably
efficient.  As Hughes and Wadler, I will aim at demonstrating best functional
style. TODO: whut? Finally,
I will draw general conclusions on how to improve on functional
programming methodologies.

@sec_api<-section«API (Syntax)»

We will start the design of our library by examining a
simple yet typical pretty printing task.  Let us assume we want to
pretty print S-Expressions, and that they are represented as follows:

@haskell«
data SExpr where
   SExpr :: [SExpr] -> SExpr
   Atom  :: String  -> SExpr
  deriving Show
»
Using the above representation, the S-Expr @teletype«(a b c d)» has the
following encoding:

@haskell«
abcd :: SExpr
abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]
»

The goal of the pretty printer is to render a given S-Expr according to the
three principles of pretty printing: @pcp_visibility, @pcp_compact and @pcp_layout.
While it is clear how the first two principles constrain the result, it
is less clear how the third principle plays out: we must specify more precisely which
layouts are admissible. To this end, we will say that in a pretty
display of an S-Expr, we would like the elements to be either
concatenated horizontally, or aligned vertically. The possible pretty
layouts of our @hask«abcd» example would be either

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

In general, a pretty printing library must provide the means to express
the specification of admissible layouts: it is up to the user to
reify @pcp_layout on the data structure of interest. The printer
will then automatically pick the smallest (@pcp_compact) layout which fits
the page (@pcp_visibility).

Our library provides an API
to decribe layouts which is similar to Hughes's: we can express both
vertical (@hask«$$») and horizontal (@hask«<>») composition of
documents, as well as embedding raw @hask«text» and provide
automatic choice between layouts (@hask«<|>»). At this stage, we keep
the representation of documents abstract, by using a typeclass which
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

(<+>)    :: Layout d => d -> d -> d
x <+> y  = x <> text " " <> y

hsep,vcat :: Doc d => [d] -> d
vcat = foldr1 ($$)
hsep = foldr1 (<+>)
»

We can furthermore define automatic choice between horizontal and
vertical concatenation:

@haskell«
sep :: Doc d => [d] -> d
sep []  = empty
sep xs  = hsep xs <|> vcat xs
»

Turning S-expressions into a pretty document is then child's play:

@haskell«
pretty :: Doc d => SExpr -> d
pretty  (Atom s)    = text s
pretty  (SExpr xs)  =   text "(" <>
                        (sep $ map pretty xs) <>
                        text ")"
»

@sec_informal_semantics<-section«Towards semantics»

Our API provies a syntax to describe documents. The natural question is then: what should
its semantics be?  In other words, how do we turn the three principles into a
specification of @hask«render»?

Let us use an example to try and answer the question, and outline why neither Wadler's or Hughes' answer is satisfactory. Suppose we want
to pretty print the following S-Expr (which is specially crafted to
demonstrate issues with both Hughes and Wadler libraries):

@haskell«
testData :: SExpr
testData = SExpr [  SExpr [Atom "12345", abcd4],
                    SExpr [Atom "12345678", abcd4]]
  where abcd4 = SExpr [abcd,abcd,abcd,abcd]
»

@fig_eighty<-figure_«
Example expression printed on 80 columns. The first line is a helper showing the column of a given character.
»«
@verbatim«
12345678901234567890123456789012345678901234567890123456789012345678901234567890

((12345 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (12345678 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
»»

Remember that would like elements inside an S-Expr to be either
aligned vertically or concatenated horizontally (for legibility,
@pcp_layout), The second option will be preferred over the first
(@pcp_compact), as long as the text fits within the page width
(@pcp_visibility).
Interpreting the above (so far informal) specification yields the
following results. 1. On a 80-column-wide page, we would get the result
displayed in @fig_eighty.
2. On a 20-column-wide page, we would like to get the following output:

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

@subsection«The liminations of Hughes and Wadler»

Let us take a moment to survey the state of the art.  On a 20-column
page and using Hughes' library, we would get the following output
instead:

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

The above output uses way more space than necessary, violating
@pcp_compact.  Why is that? Hughes states that @qu«it would be
unreasonably inefficient for a pretty-printer do decide whether or not
to split the first line of a document on the basis of the content of
the last.» (sec. 7.4 of his paper).  Therefore, he chooses a greedy
algorithm, which processes the input line by line, trying to fit as
much as possible on the current line, without regard for what comes
next.  In our example, the algorithm can fit @teletype«(12345678 ((a»
on the sixth line, but then it has committed to a very deep
indentation level, which forces to display the remainder of the
document in a narrow space, wasting space.

How does Wadler's library fare on the example? Unfortunately, we
cannot answer the question in a strict sense. Indeed, Wadler's API is
to restrictive to even @emph«express» the layout we are after! That
is, one can only specify a @emph«constant» amount of indentation, not
one that depends on the contents of a document.  This means that
Wadler's library lacks the capability to express that a multi-line
sub-document @hask«b» should be laid out to the right of a document
@hask«a» (even if @hask«a» is single-line).  Instead, @hask«b» must be
put below @hask«a». Because of this restriction, with any reasonable
specification, the best output that Wadler's library
can produce is the following:

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

It's not too bad... but there is a spurious line break after the atom
@teletype«12345678». While Wadler's restriction may be acceptable to some, I find it
unsatisfying for two reasons. First, spurious line breaks may appear
in many places, so the rendering may be much longer than necessary, thereby violating @pcp_compact.
Second, and more importantly, a document which is laid out below another cannot
be properly indented in general. Let us say we would like to
pretty print the following ml-style snippet:
@verbatim«
Pattern = expression [listElement x,
                      listElement y,
                      listElement z,
                      listElement w]
»
If the list does not fit on a single line, it must be put below
@qu"expression". For legibility, it must be indented. However, the
amount of indentation cannot depend on the @teletype«Pattern», so a typical output is:
@verbatim«
Pattern = expression
  [listElement x,
   listElement y,
   listElement z,
   listElement w]
»
Aligning the argument of the expression below the equal sign is bad:
it obscures the structure of the program; @pcp_layout is not
respected. In sum, the lack of a combinator for relative indentation
is a serious drawback. In fact, Daan Leijen's
implemenation of Wadler's design (@sans«wl-print»), @emph«does» feature
an alignment combinator. However, the implemenation also uses a greedy algorithm, and thus
suffers from the same issue as Hughes' library.

In sum, we have to make a choice between respecting all the principles
of pretty printing or provide a greedy algorithm. Hughes does not
fully respect @pcp_compact. Wadler does not fully respect
@pcp_layout. Here, I decide to respect both, but I give up on
greediness.
Yet, the final algorithm that I will arrive at is fast enough for use
in common pretty printing tasks.

But; let us not get carried away: before attacking the problem of making a fast (enough) implementation,
we need to finish the formalisation of the semantics. And before that,
it is best if we spend a moment to further refine the API for defining pretty layouts.

@section«Semantics, continued»

@subsection«Layouts»
We ignore for a moment choice between possible layouts
(@hask«<|>»). As Hughes, we call a document without choice a @emph«layout».

Recall that we have inherited from Hughes a draft API for layouts:

@spec«
text  :: Layout d => String -> d
(<>)  :: Layout d => d -> d -> d
($$)  :: Layout d => d -> d -> d
»


At this stage, classic functional pearls would state a number of laws
that the above API has to satisfy, then infer a semantics from them.
Fortunately, in our case, Hughes and Wadler have already laid out this
ground work, so we can jump straight to giving a compositional
semantics. We will later check that the expected laws hold.

Let us interpret a layout as a @emph«non-empty» list of lines to print. As
Hughes, I'll simply use the type of lists (you will remember the
invariant in your head --- don't worry, I will help).

@haskell«
type L = [String] -- non empty.
»

@hiddenHaskell«
instance Layout L where
»
Preparing a layout for printing is as easy as inserting a newline character between each string:
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
The only potential difficulty is to figure out the interpretation of
horizontal concatenation (@hask«<>»). We will stick to Hughes' advice:
@qu«translate the second operand [to the right], so that is tabs against
the last character of the first operand». Diagramatically:

@(horizCat False)

The implementation handles the last line of the first layout and the
first line of the second layout specially, as follows:

@haskell«
  (<>) :: L -> L -> L
  xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
     where  xs0 = init xs
            x = last xs
            n = length x
            indent = replicate n blankChar
»

Given the above definition, we can then refine our API a bit.
Indeed, concatenation is (nearly) a special case of horizontal composition. That is,
instead of composing vertically, one can add an empty line to the
left-hand-side layout and then compose horizontally. The combinator which adds
an empty line is called @hask«flush», and has the following definition:
@haskell«
  flush :: L -> L
  flush xs = xs ++ [""]
»
Horizontal concatenation is then:
@spec«
  ($$) :: L -> L -> L
  a $$ b = flush a <> b
»

One might argue that replacing @hask«($$)» by @hask«flush» does not
make the API shorter, and maybe does not even make it simpler. Yet, we will stick
this choice, for two reasons:

@enumList[«The new API clearly separates the concerns of concatenation and
           left-flushing documents.»
         ,«The horizontal composition (@hask«<>») has a nicer algebraic structure
          than (@hask«$$»). The vertical composition (@hask«$$») has no unit, while (@hask«<>») has empty layout as unit.
          (Due Hughes' complicated semantics,
          even his (@hask«<>») operator lacks a unit.)»]

To sum up, our API for layouts is the following:
@haskell«
class Layout d where
  (<>)    :: d -> d -> d
  text    :: String -> d
  flush   :: d -> d
  render  :: d -> String
»
Additionally, as mentioned above, layouts follow a number of algebraic
laws:

@enumList[
«Layouts form a monoid, with operator (@hask«<>») and unit @hask«empty»@footnote«recall @hask«empty = text ""»»:

@haskell«
prop_leftUnit :: (Doc a, Eq a) => a -> Bool
prop_leftUnit a = empty <> a == a

prop_rightUnit :: (Doc a, Eq a) => a -> Bool
prop_rightUnit a = a <> empty == a

prop_assoc :: (Doc a, Eq a) => a -> a -> a -> Bool
prop_assoc a b c = (a <> b) <> c == a <> (b <> c)
»»
,
«@hask«text» is a monoid homomorphism:
@spec«
prop_text_append s t  = text s <> text t == text (s ++ t)
prop_text_empty       = empty == text ""
»
»,
« @hask«flush» can be pulled out of concatenation, in this way:

@haskell«
prop_flush :: (Doc a, Eq a) => a -> a -> Bool
prop_flush a b =  flush a <> flush b == flush (flush a <> b)
»
»]


@subsection«Choice»

We proceed to extend the API with choice between layouts, yielding the
final API to specify document. The extended API is accessible via a
new type class:

@haskell«
class Layout d => Doc d where
  (<|>) :: d -> d -> d
  fail :: d
»

Again, we give the compositional semantics right away. Documents are
interpreted as a set of layouts. We implement sets as list, where
order and number of occurences won't matter.

TODO: rename F to Set
@haskell«
newtype F a = F {fromF :: [a]}
  deriving (Functor,Applicative,Show)
»

The interpretation is as one expects:
@haskell«
instance Doc (F L) where
  F xs <|> F ys = F (xs ++ ys)
  fail = []
»

Consequently, disjunction and failure form a monoid, and disjuction is
commutative. Disjunction is idempotent.

@haskell«
prop_leftUnit' :: (Doc a, Eq a) => a -> Bool
prop_leftUnit' a = fail <|> a == a

prop_rightUnit' :: (Doc a, Eq a) => a -> Bool
prop_rightUnit' a = a <|> fail == a

prop_assoc' :: (Doc a, Eq a) => a -> a -> a -> Bool
prop_assoc' a b c = (a <|> b) <|> c == a <|> (b <|> c)
»

We simply lift the layout operators idiomatically @citep"mcbride_applicative_2007" over sets:

@haskell«
instance Layout (F L) where
  text = pure . text
  flush = fmap flush
  xs <> ys = (<>) <$> xs <*> ys
»

Consequently, concatenation and @hask«flush» distribute over disjunction:

@haskell«
prop_distrl :: (Doc a, Eq a) => a -> Bool
prop_distrl a = (a <|> b) <> c == (a <> c) <|> (b <> c)

prop_distrr :: (Doc a, Eq a) => a -> Bool
prop_distrr a = c <> (a <|> b) == (c <> a) <|> (c <> b)

prop_distrflush :: (Doc a, Eq a) => a -> a -> Bool
prop_assoc' a b = flush (a <|> b) == flush a <|> flush b
»

@subsection«Semantics»

We can finally define formally what it means to render a document.  To
pretty print a document, we pick a shortest layout among the valid
ones:
@haskell«
  render =   render .  -- (for layouts)
             minimum .
             filter valid .
             fromF
»
TODO: attn. order used
A layout is @hask«valid» if all its lines are fully visible on the page:
@haskell«
valid :: L -> Bool
valid xs = maximum (map length xs) <= 80
»

@section«A More Efficient Implementation»
@subsection«Measures»

At this point, a classic functional pearl would derive an
implementation via a series of calculational steps. While this may
very well be done, I will instead proceed to follow the actual thought
process that I used when designing the library. The hope is that the
actual method is more applicable than a re-engineered story.

Let us remember that we want to select the layout with minimal use of
space. Hence, from an algorithm point of view, all that matters is the
space that a layout takes. Let us define an abstract semantics for
documents which focuses on such space.

All that matters is the maximum width of the layout, the width of its
last line and its height (and because layouts can't be empty we do not
count the last line):
@singleLayoutDiag
In code:
@haskell«
data M = M {  height     :: Int,
              lastWidth  :: Int,
              maxWidth   :: Int}
  deriving (Show,Eq,Ord)
»

Astute readers may have guessed the above semantics by looking at the diagram for
composition of layouts shown earlier. Indeed, it is the above semantics which justify
the abstract representation of a layout that the diagram uses.
Here is the concatenation
diagram annotated with those lengths.

@(horizCat True)

The above diagram can be read out as code as follows: 
@haskell«
instance Layout M where
  a <> b =
     M {  maxWidth   = max  (  maxWidth a)
                            (  lastWidth  a + maxWidth   b),
          height     =         height     a + height     b,
          lastWidth  =         lastWidth  a + lastWidth  b}
»
The other combinators are easy to implement:
@haskell«
  text s   = M {  height     = 0,
                  maxWidth   = length s,
                  lastWidth  = length s}
  flush a  = M {  maxWidth   = maxWidth a,
                  height     = height a + 1,
                  lastWidth  = 0}
»

We can even give a rendering for these abstract layouts, by printing an @teletype«x» at each
busy position in the layout.
@haskell«
  render (M lw h mw) = render $
      replicate h (replicate mw 'x') ++ [replicate lw 'x']
»

The correctness of the above code relies on the intution of and a
proper reading of the concatenation diagram. This process being
informal, so we should to cross-check the final result formally.
To do so, we define a function which computes the measure of a full layout:
@haskell«
measure :: L -> M
measure xs = M {  maxWidth   = maximum $ map length $ xs,
                  height     = length xs - 1,
                  lastWidth  = length $ last $ xs}
»

Then, to check the correctness of the @hask«Layout M» instance, we
must check that @hask«measure» is a layout homomorphism (ignoring of
course the @hask«render»er). The homomorphism property can be spelled out as the following
three laws:

@spec«
measure (a <> b) == measure a <> measure b
measure (flush a) == flush (measure a)
measure (text s) == text s
»

Checking the laws is as simple, if somewhat a tedious, exercise to the reader.

Having properly refined the problem (ignoring such details as the
actual text being rendered), we may proceed to give a fast
implementation of the pretty printer.

@haskell«
fits :: M -> Bool
fits x = maxWidth x <= 80
»


@subsection«Early filtering out invalid results»

The first optimisation is to filter out invalid results early; like so:

@spec«
text x = filter valid [text x]
xs <> ys = filter valid [x <> y | x <- xs, y <- ys]
»

We can do this because @hask«width» is monotonous:

@spec«
width (a <> b) ≥ width a  and width (a <> b) ≥ width b
width (flush a) ≥ with a
»

In turn, so is validity:

@spec«
valid (a <> b)   => valid a  ∧  valid b
valid (flush a)  => valid a
»

Consequently, keeping invalid layouts is useless: they can never be
combined with another layout to produce something valid:

@spec«
not (valid a)    => not (valid (a <> b))
not (valid b)    => not (valid (a <> b))
not (valid a)    => not (valid (flush a))
»

@subsection«Pruning out dominated results»

The second optimisation relies on the insight that certain results
dominate others, and that dominated results may be discarded early, in
a fashion similar to what we have done above.

The domination relation is a partial order (a reflexive, transitive and antisymmetric relation). We write @hask«a ≺ b» if
@hask«a» dominates @hask«b».
@haskell«
class Poset a where
  (≺) :: a -> a -> Bool
»
We set up the domination relation such that
@enumList[«It is preserved by the layout operators:
@spec«if    d1 ≺  d2 and  d'1 ≺  d'2
then  (d1 <> d2) ≺  (d'1 <> d'2) and
      flush d1 ≺ flush d2
»
»
,
«It implies the ordering layouts: @hask«a ≺ b  =>  a ≤ b».
»
]

Together, these properties mean that we can always discard dominated
layouts from a set, as we can discard invalid ones. for any @hask«f» of type @hask«Layout l => l -> l»
@spec«
a ≺  a'  => f a ≺  f a'  =>  f a <= f a'
»
The domination relation that we use is simply the
intersection of ordering in all dimensions.
@haskell«
instance Poset M where
  M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2
»

That is, if layout
@hask«a» is shorter, narrower, and has a narrower last line than
layout @hask«b», then @hask«a» dominates @hask«b».

Filtering out the dominated elements is an operation known as the
computation of the Pareto frontier, which can be implemented as
follows.

@haskell«
pareto :: Poset a => [a]  -> [a]
pareto = loop []
  where  loop acc  []      = acc
         loop acc  (x:xs)  =
            if any (≺ x) acc
               then  loop acc xs
               else  loop (x:filter (not . (x ≺)) acc) xs
»

The above function examines elements sequentially, and keeps a pareto frontier
of the elements seen so far in the @hask«acc» parameter. For each examined element @hask«x», if it
is dominated, then we merely skip it.  Otherwise, @hask«x» is added to
the current frontier, and remove all elements dominated
by @hask«x» are then removed.

The implementation is then as follows:

@haskell«
type DM = [M]

instance Layout DM where
  xs <> ys =  pareto $ concat
              [ filter fits [x <> y | y <- ys] | x <- xs]
  flush xs = pareto $ (map flush xs)
  text s = filter fits [text s]

instance Doc DM where
  xs <|> ys = pareto (xs ++ ys)
»

Unfortunately, computing the pareto frontier as above is slow when
most elements are in the pareto frontier. Indeed, adding an element
requires to re-traverse all elements in the frontier so far, and thus @hask«pareto»
has quadratic complexity.

There is a better way: keeping the lists sorted in lexicographical
order. Then the pareto fronter has a more efficient implementation.

Because the input is lexicographically sorted, everything which is in
the frontier can't be dominated by a new element. Indeed, the new
element @hask«m1» must be lexicographically bigger than any element
@hask«m0» in the frontier:

@spec«M x0 y0 z0 <= M x1 y1 z1»
which means, by definition:
@spec«
x0 < x1                          or
x0 = x1 and y0 < y1              or
x0 = x1 and y0 = y1 and z0 < z1
»

which is incompatible with @hask«m1 ≺ m0».
In sum, so no element @hask«m0» already in the frontier can be dominated by @hask«m1».

Consequently, on a sorted list, we can skip the re-filtering of the @hask«acc»umulated frontier, as follows:
@haskell«
pareto' :: Poset a => [a] -> [a]
pareto' = loop [] where
  loop acc  []      = []
  loop acc  (x:xs)  = if any (≺ x) acc
                          then     loop' acc      xs
                          else x:  loop' (x:acc)  xs
»

In order to make use of the optimised Pareto frontier algorithm, we must then ensure that the operators preserve the lexicographically
sorted property. We need in particular the ability to merge sorted lists:

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
mergeAll = foldr merge []
»


The @hask«Doc» instance can then be written simply:

@haskell«
type D1 = [M]

instance Doc D1 where
  xs <|> ys = pareto $ merge xs ys

instance Layout D1 where
»

In the implementation of the @hask«Layout» instance, we must
take care to keep lists sorted, in particular in the concatenation operation.
Let us propose the following implementation:

@haskell«
  xs <> ys = pareto' $ mergeAll [ filter fits [x <> y | y <- ys] | x <- xs]
»

For each of the inner list comprehensions to generate sorted output, we need the following lemma.

@lemma«»«
@spec«
if    d1 <=  d2   then  (d <> d1) <=  (d <> d2)
»»«

The proof works precisely because we have chosen a suitable order for the fields in @hask«M».

That is, if we have

@hask«M h2 lw2 mw2 <= M h2' lw2' mw2'»

then we must have:

@hask«M (h1+h2) (lw1+lw2) (max mw1 (mw2 + lw1)) <= M (h1+h2') (lw1+lw2') (max mw1 (mw2' + lw1))»

This last condition is satisfied iff.:

@spec«
h1+h2 <  h1+h2'                               or
h1+h2 == h1+h2'  and  lw1+lw2 <  lw1+lw2'     or
h1+h2 == h1+h2'  and  lw1+lw2 == lw1+lw2'
                 and max mw1 (lw1+mw2) <=  max mw1 (lw1+mw2')»

All but the last sub-condition follows the struture of the hypothesis, and thus is satisfied.
To deal with the that last bit (@hask«max mw1 (lw1+mw2) <=  max mw1 (lw1+mw2')»), we do a case analysis.


@enumList[«@hask«lw1 + mw2 <= mw1»:
          The condition simplifies to:
          @hask«mw1 <=  max mw1 (lw1+mw2')»
          which holds by definition of @hask«max».»
          ,
          «@hask«lw1 + mw2 > mw1»:,
          The condition simplifies to:
          @hask«lw1+mw2 <=  max mw1 (lw1+mw2')»
          it suffices then to show
          @hask«lw1+mw2 <=  lw1+mw2'»
          or just
          @hask«mw2 <= mw2'»
          which is given by hypothesis when @hask«h2 == h2'» and  @hask«lw2 == lw2'».»
          ]
»


Flush does not, so we have to re-sort the list.


@haskell«
bests = pareto' [] . mergeAll
»

@haskell«
  -- flush xs = pareto' $ mergeAll $ map sort $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = pareto' $ sort $ (map flush xs)
  text s = [text s | valid (text s)]
  render (x:_) = render x
»


@subsection«Re-pairing with text»

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

@subsection«Hughes-Style nesting»

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

@subsection«Ribbon length»

Note that we pick the narrowest result fitting on min. lines lines!

@subsection«Using something better than strings for text»

@subsection«Laws vs compositional semantics»

Note that laws may only @emph«partially» specify the behaviour, while a
semantic model will always fully constrain it.

(exercise: does the above set of laws fully constrain the semantic model?)

Notice that Hughes and Wadler give the semantics via laws first and
come up with a compositional interpretation second. This is fine,
precisely because laws do not fully constrain the design; there is
room for wiggle. However, a compositional semantics is often an even
better guide which should not be an afterthought.

@acknowledgements«Using the QuickSpec tool, Nicholas Smallbone helped
finding a bug in the final implementation: the concatenation operator
did not preserve the invariant that lists were sorted. »

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


abstractLayoutJoin (a,a_last) (b,b_last) = do
  j <- boundingBox [a,b]
  b # NW .=. a_last
  return (j,b_last)

asse a = a # SW + Point 0 lineHeight

lw1 = 12
lw2 = 18

showDot sz color p =
  using (outline color) $ do
    c <- circleShape
    c#Center .=. p
    width c === sz

dblarrow :: Box -> Point -> Point -> Diagram ()
dblarrow lab a b = do
   let points = [a,b]
       normal = OVector (avg points) (rotate90 (b-a))
       p = polyline points
   using (outline "black" . set endTip ToTip  . set startTip ToTip) $ path p
   tighten 10 $ autoLabel lab normal
   -- showDot 1 "black" (avg points)
   -- showDot 2 "green" (lab # Center)
   -- showDot 3 "red" (avg points + rotate90 (b-a))
   -- using (outline "blue") $ rectangleShape $ anchors lab
   return ()

hruler = gruler xpart ypart
vruler = gruler ypart xpart

gruler xp yp p1 p2 lab = do
  p1' <- point
  p2' <- point
  l <- labelObj lab
  align xp [p1,p1']
  align xp [p2,p2']
  align yp [p1',p2']
  dblarrow l p1' p2'
  -- stroke "red" $
  boundingBox [anchors p1', anchors p2', anchors l]

rulersOfLayout l mw lw (a,mid) = do
  heightRule <- vruler (a # N) (asse a) (hask l)
  a `leftOf` heightRule

  mwRule <- hruler (a # W) (a # E) (hask mw)
  mwRule `topOf` a

  lwRule <- hruler mid (a # W) (hask lw)
  a `topOf` lwRule
  return (heightRule,mwRule,lwRule)


singleLayoutDiag :: Tex ()
singleLayoutDiag = center $ element $ do
  a@(bx,_) <- draw $ abstrLayout 40
  width bx === 56
  height bx === 7 *- lineHeight
  rulersOfLayout «height» «maxWidth» «lastWidth» a
  return ()

twoLayouts :: Diagram ((Object,Point),(Object,Point))
twoLayouts = do
  (a,a_last) <- abstrLayout lw1
  (b,b_last) <- abstrLayout lw2
  width a === 48
  width b === 56
  height a === 4 *- lineHeight
  height b === 3 *- lineHeight

  return ((a,a_last),(b,b_last))


horizCat :: Bool -> TeX
horizCat showRulers = center $ element $ do
  (a,b) <- draw $ twoLayouts
  op <- labelObj "<>"
  let lhsObjs = [fst a,op, fst b]
  align ypart $ [fst a # N, fst b # N]
  align ypart $ [fst a # Center, op # Center]
  
  eq <- labelObj "="
  abSep <- draw $ twoLayouts
  ab <- uncurry abstractLayoutJoin $ abSep
  (lhsObjsExtra,rhsObjsExtra) <- if showRulers
    then do
      (h1,_,lw1) <- rulersOfLayout «l1» «mw1» «lw1» a
      (_,_,_lw2) <- rulersOfLayout «l2» «mw2» «lw2» b

      (_hTot,mwTot,lwTot) <- rulersOfLayout «l1+l2» «max mw1 (lw1+mw2)» «lw1+lw2» ab
      spread hdist 10 [h1,op,fst b]
      return ([lw1],[mwTot,lwTot])
    else do
      spread hdist 10 lhsObjs
      return ([],[])
  spread hdist 10 [eq, fst ab]
  align ypart $ map (#Center) [fst ab, eq]
  lhs <- boundingBox $ lhsObjs ++ lhsObjsExtra
  rhs <- boundingBox $ [eq, fst ab] ++ rhsObjsExtra
  align xpart $ map (#W) [rhs,lhs]
  spread vdist 10 [rhs,lhs]

  return ()


spec = haskell
hiddenHaskell = haskell

verbatim :: Verbatim () -> TeX
verbatim (Verbatim s _) =
    env "verbatim" (tex s)

-- hask = ensureMath . cmd "mathsf"
hask :: Verbatim a -> Tex ()
hask = ensureMath . haskellInline


url :: TeX -> TeX
url x = do
  usepkg "url" 100 []
  cmd "url" x

displayLeft :: Tex a -> Tex a
displayLeft body = env'' "list" [] [mempty,tex "\\setlength\\leftmargin{1em}"] $ do
  texLn "\\item\\relax"
  body

display :: Tex a -> Tex a
display = env "center"

footnote = cmd "footnote"
