{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (deflike, thmlike, definition, mathpreamble,lemma,theorem)
import MarXup.Tex hiding (label)
import MarXup.Diagram hiding (height)
import qualified MarXup.Diagram as D
import MarXup.LineUp.Haskell
import MarXup.Verbatim
import MarXup.Latex.Math (ensureMath)
import Control.Lens (set)
import Data.Function
import Data.List (intercalate, minimumBy, sort, groupBy)
-- import System.Clock
import Prelude hiding (fail,Num(..),(/),(^))
import Control.Monad (forM_,when,forM)
import Graphics.Diagrams.Plot
import System.IO.Unsafe (unsafePerformIO)
import Numeric (showFFloat, showEFloat)
import Criterion (nf)
import qualified Criterion.Main.Options as C
import qualified Criterion.Monad as C
import Criterion.Types (Measured(..), Report(..), SampleAnalysis(..), Benchmarkable)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Criterion.Internal (runAndAnalyseOne)
import System.Environment (getArgs)
import Algebra.Classes

($$) :: forall l. Layout l => l -> l -> l
a $$ b = flush a <> b

newpage :: TeX
newpage = cmd0 "newpage"

-- benchmark :: forall a. (Eq a, Num a) => a -> Benchmarkable
benchmark size = nf testOne size

instance Element String where
  type Target String = TeX
  element = textual

-- testOne :: forall a. (Eq a, Num a) => a -> Int
testOne size = height mm
    where mm :: M
          mm = minimum $ (pretty input :: DM)
          -- mm' :: M'
          -- mm' = minimum $ (pretty input :: [M'])
          D1 (mm1:_) = pretty input
          l :: String
          l = render $ (pretty input :: [L])
          input = testExpr size

dataFileName :: FilePath
dataFileName = "benchmark-" ++ show pageWidth ++ ".dat"

performanceData :: [(Integer, Integer, (Double,Double,Double))]
performanceData = unsafePerformIO $ do
  d <- readFile dataFileName
  return [(sz,h,(estLowerBound e, estPoint e, estUpperBound e)) | (sz,h, e) <- read d]
  -- forM [1..15] $ \size -> do
  --   (Measured { measTime = dt},_) <- C.measure (benchmark size) 1
  --   return (size,2 ^ (max 0 (size - 2)), dt)
  --   -- time $ show mm1
  --   -- time $ show mm'

regimeSpeed :: Double
regimeSpeed = fromIntegral nlines / time
  where ((_,nlines,(_,time,_)):_) = reverse performanceData

performanceAnalysis :: IO ()
performanceAnalysis = do
  an <- C.withConfig C.defaultConfig $ do
    forM [1..15] $ \size -> do
      (Report { reportAnalysis = SampleAnalysis {anMean = dt}}) <- runAndAnalyseOne size ("bench " ++ show size) (benchmark size)
      return (size,(2::Integer) ^ (max 0 (fromIntegral size - 2)), dt)
  writeFile dataFileName $ show an


performanceTable :: TeX
performanceTable = tabular [] "rrr" [[textual (show s), textual (show h),textual (show t)] | (s,h,t) <- performanceData]

performancePoints :: [Point' Double]
performancePoints = [x | (_,x,_) <- performanceBars]

performanceBars :: [(Point' Double,Point' Double,Point' Double)]
performanceBars = [(Point x l, Point x m, Point x h)
                  | (_,nlines,(l,m,h)) <- performanceData, let x = fromIntegral nlines]



scatterWithErrors :: PlotCanvas a -> [(Vec2 a,Vec2 a,Vec2 a)] -> TexDiagram ()
scatterWithErrors (bx,t) inputs = do
  let three f (a,b,c) = (f a, f b, f c)
  forM_ (map (three (interpBox bx . (forward <$> t <*>))) inputs) $ \(_l,m,h) -> do
    -- draw $ path $ polyline [l,h]
    -- stroke "red" $ path $ polyline [m - Point 3 0, m + Point (-3) 0]
    showDot (constant 2) "black" m
    -- forM [l,h] $ \z -> stroke "red" $ path $ polyline [z - Point 2 0, z + Point 2 0]
    -- error bars are so narrow that we do not see them.

performancePlot :: Vec2 (ShowFct TeX Double) -> Vec2 (Transform Double) -> Diagram TeX Tex ()
performancePlot sho axes =  do
  let points' = sequenceA performancePoints
  c@(bx,_) <- preparePlot sho axes (minimum <$> points') (maximum <$> points')
  scatterWithErrors c performanceBars
  functionPlot c 5 (\x -> x/regimeSpeed)
  width bx === constant 200
  D.height bx === constant 100

renderFloat :: forall a. RealFloat a => a -> Tex ()
renderFloat x = tex $ showEFloat (Just 0) x ""

performancePlotLog, performancePlotLin :: Diagram TeX Tex ()
performancePlotLog = performancePlot (pure renderFloat) (Point (logAxis 10) (logAxis 10))
performancePlotLin = performancePlot (pure renderFloat) (Point (simplLinAxis 2000) (simplLinAxis 0.5))


-- data ErrorBar a = ErrorBar {lbound, mean, ubound :: a}

tm :: Verbatim a -> Tex ()
tm x = do
  tex "$"
  tex $ fromVerbatim x
  tex "$"
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["benchmark"] -> performanceAnalysis
    _ -> renderTex SIGPlan "Prettiest" (preamble (header >> mainText >> bibliographyAll >> appendix))

comment :: TeX -> TeX
comment _ = mempty

preamble body = do
  documentClass "../PaperTools/latex/sigplanconf" ["preprint"]
  stdPreamble
  mathpreamble
  cmd "input" $ tex "../PaperTools/latex/unicodedefs"

  title "A pretty but not greedy printer"
  authorinfo [AuthorInfo "Jean-Philippe Bernardy" "bernardy@chalmers.se" "CTH"]
  env "document" body

principle :: TeX -> TeX -> Tex TeX
principle titl body = do
  deflike "Principle" "principle" "Principle" (smallcaps titl) body
  return $ smallcaps titl


header :: Tex ()
header = do
  maketitle
  abstract
  keywords $ [ "Pearl", "Pretty Printing"]
  return ()

bibliographyAll :: TeX
bibliographyAll = do
  bibliographystyle "abbrvnat"
  bibliography  "../PaperTools/bibtex/jp"

abstract = env "abstract" «
  This paper proposes an new specification of pretty printing which is stronger than the state of the art:
we require the output to be the shortest possible, and we also offer the ability to align sub-documents at will.
We argue that specification precludes a greedy implementation. Yet,
we provide an implementation which behaves linearly in the size of the output.
This implementation is arrived at by using orthodox functional programming methodology.
»


mainText :: Tex ()
mainText = «


@sec_intro<-section«Introduction»



A pretty printer is a program that prints data structures in a way which
makes them pleasant to read. (The data structures in question
often represent programs, but not always.)
Pretty printing has historically been used by the functional programming
community to showcase proper style. The pretty printer of @citet"hughes_design_1995"
remains an influential example of functional programming design, while that of
@citet"wadler_prettier_2003" was published as a chapter in a book dedicated to the @qu"fun of programming".

In addition to their esthetical and pedagogical value, the pretty printers of Hughes and Wadler
are practical implementations which form the basis of industrial-strength pretty-printing packages, which remain popular today. Hughes' design has been
refined by Peyton Jones, and is available as the
Hackage package @sans«pretty»@footnote«@url«https://hackage.haskell.org/package/pretty»»,
while Wadler's design has been extended by Leijen and made available as the
@sans«wl-print» package@footnote«@url«https://hackage.haskell.org/package/wl-pprint»».

This paper is a bold attempt to improve some aspects of the aforementioned landmark pieces of work in the functional programming landscape.
Yet, our goal is slightly different to that of Hughes and Wadler. Indeed, they aim first and formost to
demonstrate general principles of functional programming development, with an emphasis on the efficency of the algorithm.
The methodological idea is to derive a greedy algorithm from a functional specification.
In the process, they give themselves some leeway in what they accept as pretty outputs (see #sec_notSoPretty).
In contrast, my primary goal is to produce @emph«the prettiest output», at the cost of efficiency. Yet, we the final result is reasonably
efficient (@sec_timings).

Let specify the desired behaviour of a pretty printer, first informally, as the following principles:

@pcp_visibility<-principle«Visibility»«A pretty printer shall
layout all its output within the width of the page.»

@pcp_layout<-principle«Legibility»« A pretty printer shall make clever use of layout, to make it easy
             for a human to recognise the hierarchical organisation of data.»

@pcp_compact<-principle«Frugality»«A pretty printer shall minimize the number of lines
used to display the data.»

Furthermore, the first principle takes precedence over the second one, which itself takes precedence over the third one.

In the rest of the paper, we interpret the above three principles as an optimisation problem, and derive a program
which solves it efficiently enough.

@sec_api<-section«Interface (Syntax)»

Let us use an example to guide the development of our pretty-printing interface.
Assume that we want to pretty print S-Expressions, and that they are represented as follows:

@haskell«
data SExpr = SExpr [SExpr] | Atom String
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
layouts are admissible. To this end, we assert that in a pretty
display of an S-Expr, we would like the elements to be either
concatenated horizontally, or aligned vertically. Thus the legible
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
the set of legible layouts: it is up to the user to
reify @pcp_layout on the data structure of interest. The printer
will then automatically pick the smallest (@pcp_compact) legible layout which fits
the page (@pcp_visibility).

Our layout-description API
is similar to Hughes's: we can express both
vertical (@hask«$$») and horizontal (@hask«<>») composition of
documents, as well as embedding raw @hask«text» and provide
choice between layouts (@hask«<|>»). At this stage, we keep
the representation of documents abstract, by using a typeclass (@hask«Doc») which
provides the above combinators, as well as means of @hask«render»ing a
document:

@spec«
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
x <+> y  = x <> text " " <> y

hsep,vcat :: Doc d => [d] -> d
vcat  = foldr1 ($$)
hsep  = foldr1 (<+>)
»

We can furthermore define the choice between horizontal and
vertical concatenation:

@haskell«
sep :: Doc d => [d] -> d
sep []  = empty
sep xs  = hsep xs <|> vcat xs
»

Turning S-expressions into a @hask«Doc» is then child's play:

@haskell«
pretty :: Doc d => SExpr -> d
pretty  (Atom s)    = text s
pretty  (SExpr xs)  =   text "(" <>
                        (sep $ map pretty xs) <>
                        text ")"
»

@sec_informal_semantics<-section«Semantics: an example»

The above API provides a syntax to describe layouts. The natural question is then: what should
its semantics be?  In other words, how do we turn the three principles into a
formal specification? In particular, how do we turn the above @hask«pretty» function into a pretty printer of S-Expressions?

Let us use an example to try and answer the question, and outline why neither Wadler's or Hughes' answer is satisfactory. Suppose we want
to pretty-print the following S-Expr (which is specially crafted to
demonstrate issues with both Hughes and Wadler libraries):

@haskell«
testData :: SExpr
testData = SExpr [  SExpr [Atom "abcde", abcd4],
                    SExpr [Atom "abcdefgh", abcd4]]
  where abcd4 = SExpr [abcd,abcd,abcd,abcd]
»

@fig_eighty<-figure_«
Example expression printed on 80 columns. The first line is a helper showing the column of a given character.
»«
@verbatim«
12345678901234567890123456789012345678901234567890123456789012345678901234567890

((abcde ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
»»

Remember that we would like elements inside an S-Expr to be either
aligned vertically or concatenated horizontally (for
@pcp_layout), The second option will be preferred over the first
(@pcp_compact), as long as the text fits within the page width
(@pcp_visibility).
Interpreting the above (still informal) specification yields the
following results. 1. On a 80-column-wide page, we would get the result
displayed in @fig_eighty.
2. On a 20-column-wide page, we would like to get the following output (the first line is a helper showing the column of a given character):
@newpage
@verbatim«
12345678901234567890

((abcde ((a b c d)
         (a b c d)
         (a b c d)
         (a b c d)
         (a b c d)))
 (abcdefgh
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d))))
»

@sec_notSoPretty<-subsection«The liminations of Hughes and Wadler»

Let us take a moment to survey the state of the art.  On a 20-column
page and using Hughes' library, we would get the following output
instead:

@verbatim«
12345678901234567890
((abcde ((a b c d)
         (a b c d)
         (a b c d)
         (a b c d)
         (a b c d)))
 (abcdefgh ((a
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
much text as possible on the current line, without regard for what comes
next.  In our example, the algorithm can fit @teletype«(abcdefgh ((a»
on the sixth line, but then it has committed to a very deep
indentation level, which forces to display the remainder of the
document in a narrow area, wasting vertical space.

How does Wadler's library fare on the example? Unfortunately, we
cannot answer the question in a strict sense. Indeed, Wadler's API is
too restrictive to even @emph«express» the layout that we are after. That
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
((abcde
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d)))
 (abcdefgh
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)
   (a b c d))))
»

It does not look too bad --- but there is a spurious line break after the atom
@teletype«abcdefgh». While Wadler's restriction may be acceptable to some, I find it
unsatisfying for two reasons. First, spurious line breaks may appear
in many places, so the rendering may be much longer than necessary, thereby violating @pcp_compact.
Second, and more importantly, a document which is laid out after another cannot
be properly indented in general. Let us say we would like to
pretty print a ml-style equation composed of a @teletype«Pattern» and the following right-hand-side:
@verbatim«
expression [listElement x,
            listElement y,
            listElement z,
            listElement w]
»
We reasonably hope to obtain the following result, which puts the list to the
right of the @teletype«expression», best respecting @pcp_layout by clearly showing that the list is an argument of @teletype«expression»:
@verbatim«
Pattern = expression [listElement x,
                      listElement y,
                      listElement z,
                      listElement w]
»
However, using Wadler's library, the indentation of the list can only
be constant, so even with the best layout specification we would obtain instead
the following output:
@verbatim«
Pattern = expression
  [listElement x,
   listElement y,
   listElement z,
   listElement w]
»
Aligning the argument of the expression below to the left of the equal sign is bad, because
it needlessly obscures the structure of the program; @pcp_layout is not
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
Yet, the final algorithm that I arrive at is fast enough for
common pretty-printing tasks.

But; let us not get carried away: before attacking the problem of making an implementation,
we need to finish the formalisation of the semantics. And before that,
it is best if we spend a moment to further refine the API for defining pretty layouts.

@section«Semantics, continued»

@subsection«Layouts»
We ignore for a moment the choice between possible layouts
(@hask«<|>»). As Hughes, we call a document without choice a @emph«layout».

Recall that we have inherited from Hughes a draft API for layouts:

@spec«
text  :: Layout l => String -> l
(<>)  :: Layout l => l -> l -> l
($$)  :: Layout l => l -> l -> l
»


At this stage, classic functional pearls would state a number of laws
that the above API has to satisfy, then infer a semantics from them.
Fortunately, in our case, Hughes and Wadler have already laid out this
ground work, so we can take a shortcut and immediately state a compositional
semantics. We will later check that the expected laws hold.

Let us interpret a layout as a @emph«non-empty» list of lines to print. As
Hughes, I'll simply use the type of lists, trusting the reader to remember the invariant.

@haskell«
type L = [String] -- non empty.
»

@haskell_hidden«
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
@spec«
  ($$) :: L -> L -> L
  xs $$ ys = xs ++ ys
»
The only potential difficulty is to figure out the interpretation of
horizontal concatenation (@hask«<>»). We will stick to Hughes' advice:
@qu«translate the second operand [to the right], so that is tabs against
the last character of the first operand». For example:

@verbatim«
xxxxxxxxxxxxx       yyyyyyyyyyyyyyyyyyyyy
xxxxxxxxx       <>  yyyyyyyyyyyyyyyyyyyyyyyyyyyyy
xxxxxxxxxxxx        yyyy
xxxxxx

=   xxxxxxxxxxxxx
    xxxxxxxxx
    xxxxxxxxxxxx
    xxxxxxyyyyyyyyyyyyyyyyyyyyy
          yyyyyyyyyyyyyyyyyyyyyyyyyyyyy
          yyyy
»

Or, diagramatically:

@(horizCat False)

Algorithmically, one must handle the last line of the first layout and the
first line of the second layout specially, as follows:

@haskell«
  (<>) :: L -> L -> L
  xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
     where  xs0 = init xs
            x = last xs
            n = length x
            indent = replicate n ' '
»


We take a quick detour to refine our API a bit.
Indeed, as it becomes clear with the above definition, vertical concatenation is (nearly)
a special case of horizontal composition. That is,
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
make the API shorter nor simpler. Yet, I stick
this choice, for two reasons:

@enumList[«The new API clearly separates the concerns of concatenation and
           left-flushing documents.»
         ,«The horizontal composition (@hask«<>») has a nicer algebraic structure
          than (@hask«$$»). Indeed, the vertical composition (@hask«$$») has no unit, while (@hask«<>») has the empty layout as unit.
          (In Hughes' pretty-printer, not even @hask«(<>)» has a unit, due to more involved semantics.)»]

To sum up, our API for layouts is the following:
@haskell«
class Layout l where
  (<>)    :: l -> l -> l
  text    :: String -> l
  flush   :: l -> l
  render  :: l -> String
»
Additionally, as mentioned above, layouts follow a number of algebraic
laws, (written here as QuickCheck properties):

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
One might expect this law to hold instead: @spec«a <> flush b == flush (a <> b)».
However, the inner @hask«flush» on @hask«b» goes back to the local indentation level, while the outer @hask«flush» goes back to the outer indentation level, which are equal only if @hask«a» ends with an empty line. In turn this condition is guaranteed only when @hask«a» is itself flushed.

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
interpreted as a set of layouts. We implement sets as lists, where
order and number of occurences won't matter.

The interpretation of disjunction merely appends the list of possible layouts:
@haskell«
instance Doc [L] where
  xs <|> ys = (xs ++ ys)
  fail = []
»

Consequently, disjunction is associative.

@haskell«

prop_disj_assoc :: (Doc a, Eq a) => a -> a -> a -> Bool
prop_disj_assoc a b c = (a <|> b) <|> c == a <|> (b <|> c)
»

@comment«prop_leftUnit' :: (Doc a, Eq a) => a -> Bool
prop_leftUnit' a = fail <|> a == a

prop_rightUnit' :: (Doc a, Eq a) => a -> Bool
prop_rightUnit' a = a <|> fail == a
»

We simply lift the layout operators idiomatically @citep"mcbride_applicative_2007" over sets:

@haskell«
instance Layout [L] where
  text = pure . text
  flush = fmap flush
  xs <> ys = (<>) <$> xs <*> ys
»

Consequently, concatenation and @hask«flush» distribute over disjunction:

@spec«
prop_distrl :: (Doc a, Eq a) => a -> Bool
prop_distrl a = (a <|> b) <> c == (a <> c) <|> (b <> c)

prop_distrr :: (Doc a, Eq a) => a -> Bool
prop_distrr a = c <> (a <|> b) == (c <> a) <|> (c <> b)

prop_distrflush :: (Doc a, Eq a) => a -> a -> Bool
prop_distrflush a b = flush (a <|> b) == flush a <|> flush b
»

@subsection«Semantics»

We can finally define formally what it means to render a document.  To
do so, we pick a frugal layout among the visiible
ones, according to @pcp_visibility:
@haskell«
  render =   render .  -- (for layouts)
             mostFrugal .
             filter valid -- TODO: s/valid/visible
»

A layout is @hask«valid» if all its lines are fully valid on the page:
@haskell«
    where
          valid :: L -> Bool
          valid xs = maximum (map length xs) <= pageWidth

          mostFrugal :: [L] -> L
          mostFrugal = minimumBy (compare `on` length)

pageWidth = 80
»

One may expect that disjuction should also be commutative.
However, the implementation of @hask«mostFrugal» only picks @emph«one» of
the most frugal layouts. That is fine, as all most frugal layouts are
equally good. However it also means that re-ordering the arguments of a disjunction may
affect the layout being picked. Therefore, commutativity of disjunction holds
only up to the length of the layout being rendered:

@haskell«
prop_disj_commut :: (Doc a, Eq a) => a -> a -> a -> Bool
prop_disj_commut a b c = a <|> b =~ b <|> a

infix 3 =~
(=~) :: Layout a => a -> a -> Bool
(=~) = (==) `on` (length . lines . render)
»

We have now defined semantics compositionally; furthermore this semantics is executable.
Consequently, we can implement the pretty printing an S-Expr as follows:

@haskell«
showSExpr x = render (pretty x :: [L])
»

Running @hask«showSExpr» on our example (@hask«testData») yields the expected output.

While the above semantics provide an executable implementation, it is insanely slow.
Indeed, every possible combination of choices is first constructed, and only then a shortest output is
picked. Thus, for an input with @ensureMath«n» choices, the running time is @tm«O(2^n)».


@section«A More Efficient Implementation»
@subsection«Measures»

The first insight to arrive at an efficient implementation is that it is
not necessary to construct layouts fully: only some of their parameters are relevant.
Let us remember that we want to sift through layouts based on the space that they take.
Hence, from an algorithmic point of view, all that matters is a measure of that space.
Let us define an abstract semantics for
layouts, which ignores the text, and captures only the amount of space used.

The only parameters that matter are the maximum width of the layout, the width of its
last line and its height (and, because layouts cannot be empty and it's convenient to start at zero, we do not
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
composition of layouts shown earlier. Indeed, it is the above abstract semantics (@hask«M») which justifies
the abstract representation of a layout that the diagram uses (a box with an odd last line).
Here is the concatenation diagram annotated with those lengths:

@(horizCat True)

The above diagram can be read out as Haskell code, as follows:
@haskell«
instance Layout M where
  a <> b =
     M {  maxWidth   =  max  (  maxWidth a)
                             (  lastWidth  a + maxWidth   b),
          height     =  height     a + height     b,
          lastWidth  =  lastWidth  a + lastWidth  b}
»
The other layout combinators are easy to implement:
@haskell«
  text s   = M {  height     = 0,
                  maxWidth   = length s,
                  lastWidth  = length s}
  flush a  = M {  maxWidth   = maxWidth a,
                  height     = height a + 1,
                  lastWidth  = 0}
»

We can even give a rendering for these abstract layouts, by printing an @teletype«x» at each
occupied position, completing the class instance:
@haskell«
  render m = intercalate "\n"
      (replicate (height m) (replicate (maxWidth m) 'x') ++
      [replicate (lastWidth m) 'x'])
»

The correctness of the above instance relies on intution, and a
proper reading of the concatenation diagram. This process being
informal, we should cross-check the final result formally.
To do so, we define a function which computes the measure of a full layout:
@haskell«
measure :: L -> M
measure xs = M {  maxWidth   = maximum $ map length $ xs,
                  height     = length xs - 1,
                  lastWidth  = length $ last $ xs}
»

Then, to check the correctness of the @hask«Layout M» instance, we
verify that @hask«measure» is a layout homomorphism (ignoring of
course the @hask«render»er). The homomorphism property can be spelled out as the following
three laws:

@lemma«Measure is a Layout-homomorphism»«
@spec«
measure (a <> b)   == measure a <> measure b
measure (flush a)  == flush (measure a)
measure (text s)   == text s
»
(Note: on the lhs of the above equations,
the combinators (@hask«<>, flush, text») come from
the @hask«L» instance of @hask«Layout», while on the rhs come from the @hask«M» instance.)

»«
Checking the laws is a simple, if somewhat a tedious, exercise in program calculation deferred to the appendix.
»


@haskell«
validMeasure :: M -> Bool
validMeasure x = maxWidth x <= pageWidth
»

Having properly refined the problem, and ignoring puny details such as the
actual text being rendered, we may proceed to give a fast
implementation of the pretty printer.

@subsection«Early filtering out invalid results»

The first optimisation is to filter out invalid results early; like so:

@spec«
text x = filter valid [text x]
xs <> ys = filter valid [x <> y | x <- xs, y <- ys]
»

We can do so because @hask«validity» is monotonous:

@lem_valid_mono<-lemma«@hask«valid» is monotonous»«
@spec«
valid (a <> b)   => valid a  ∧  valid b
valid (flush a)  => valid a
»»«
@spec«
valid (a <> b)   => maxWidth (a <> b) < pageWidth
                 => max (maxWidth a) (lastWidth a +  maxWidth b) < pageWidth
                 => maxWidth a < pageWidth   ∧  lastWidth a +  maxWidth b < pageWidth
                 => maxWidth a < pageWidth   ∧                 maxWidth b < pageWidth
                 => valid a  ∧  valid b


valid (flush a)  => maxWidth a < pageWidth
                 => maxWidth a < pageWidth
                 => valid a
  »
  »

Consequently, keeping invalid layouts is useless: they can never be
combined with another layout to produce something valid.

@lemma«Invalid layouts cannot be fixed»«
@spec«
not (valid a)    => not (valid (a <> b))
not (valid b)    => not (valid (a <> b))
not (valid a)    => not (valid (flush a))
»»«By contraposition of @lem_valid_mono»

@subsection«Pruning out dominated results»

The second optimisation relies on the insight that even certain valid results
are dominated by others; that is, they can be discarded early.

We write @hask«a ≺ b» when @hask«a» dominates @hask«b». We will arrange
our domination relation such that
@enumList[«Layout operators are monotonous with respect to domination.
           Consequently, for any document context

           @hask«ctx :: Doc d => d -> d», if @hask«a ≺ b» then @hask«ctx a ≺ ctx b»»
          ,«If @hask«a ≺ b», then @hask«a» is at least as frugal as @hask«b».»]

Together, these properties mean that we can always discard dominated
layouts from a set, as we could discard invalid ones. Indeed, we have:
@theorem«domination»«
a ≺  b  => ctx a ≺  ctx b  =>  height (c a) <= height (c b)
»«»

We can proceed by defining our relation and proving its properties 1. and 2. above.
We first remark that the domination relation is a partial order
(a reflexive, transitive and antisymmetric relation), and thus make it an instance
of the following class:
@haskell«
class Poset a where
  (≺) :: a -> a -> Bool
»

The order that we use is the
intersection of ordering in all dimensions: if layout
@hask«a» is shorter, narrower, and has a narrower last line than
layout @hask«b», then @hask«a» dominates @hask«b».

@haskell«
instance Poset M where
  m1 ≺ m2 =   height     m1 <= height     m2 &&
              maxWidth   m1 <= maxWidth   m2 &&
              lastWidth  m1 <= lastWidth  m2
»

The second desired property is a direct consequence of the definition.
The first one is broken down into the two following lemmas:

@lemma«@hask«flush» is monotonic»«
  if @spec«      m1 ≺  m2 » then   @spec«flush m1 ≺ flush m2 »»«
We have:
@spec«
height     m1 <= height     m2
maxWidth   m1 <= maxWidth   m2
lastWidth  m1 <= lastWidth  m2
»
and we need to prove the following three conditions

@spec«
height     (flush m1) <= height     (flush m2)
maxWidth   (flush m1) <= maxWidth   (flush m2)
lastWidth  (flush m1) <= lastWidth  (flush m2)
»

by definition, they reduce to the following inequalities, which are easy consequences of the assumptions.

@spec«
height     m1 + 1  <= height     m2 + 1
maxWidth   m1      <= maxWidth m2
0                  <= 0
»


»

@lemma«concatenation is monotonic»«
@spec«if    m1 ≺  m2 and  m'1 ≺  m'2   => (m1 <> m'1) ≺  (m2 <> m'2)  »

»«
We have:
@spec«
height     m1 <= height     m2      (1)
maxWidth   m1 <= maxWidth   m2      (2)
lastWidth  m1 <= lastWidth  m2      (3)
height     m'1 <= height     m'2    (4)
maxWidth   m'1 <= maxWidth   m'2    (5)
lastWidth  m'1 <= lastWidth  m'2    (6)
»
and we need to prove the following three conditions:
@spec«
height     (m1 <> m'1) <= height     (m2 <> m'2)
maxWidth   (m1 <> m'1) <= maxWidth   (m2 <> m'2)
lastWidth  (m1 <> m'1) <= lastWidth  (m2 <> m'2)
»
Those are by definition equivalent to the following ones:
@spec«
height     m1 + height m'1 <= height     m2 + height m'2
max (maxWidth m1) (lastWidth m1 + maxWidth m'1) <= max (maxWidth m2) (lastWidth m2 + maxWidth m'2)
lastWidth  m1 + lastWidth m'1 <= lastWidth  m2 + lastWidth m'2
»

The 1st and 3rd inequalities are consequences of the assumptions combined with the monotonicity of @hask«+».
The 2nd inequation can be obtained likewise, with additionally using the monotonicity of @hask«max»:
@spec«
a <= b ∧ c <= d   =>  max a c <= max b d
»

»



@subsection«Pareto frontier»
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

The implementation of the pretty-printing combinator then becomes:

@haskell«
type DM = [M]

instance Layout DM where
  xs <> ys =  pareto $ concat
              [ filter validMeasure [x <> y | y <- ys] | x <- xs]
  flush xs = pareto $ (map flush xs)
  text s = filter validMeasure [text s]
  render = render . minimum

instance Doc DM where
  fail = []
  xs <|> ys = pareto (xs ++ ys)
»

@sec_timings<-section«Timings»

In order to benchmark our pretty printer on large but representative outputs, we have used it to lay out
S-expressions representing full binary trees of increasing depth, as generated by the following function:

@haskell«
testExpr 0 = Atom "a"
testExpr n = SExpr [testExpr (n-1),testExpr (n-1)]
»

The set of layouts were given by using the pretty printer for S-Expressions
shown above. The most efficient version of the pretty-printer was used.
The then measured the time to compute the length of the layout. (Computing the length is enough to force the computation of the best layout.)
This benchmark heavily exercises the disjuction construct. Indeed, for each SExpr with two
sub-expressions, the printer introduces a choice. Hence for printing @hask«testExpr n»,
the pretty printer is offered @tm«2^n-1» choices,
for a total of @tm«2^{2^n-1}» possible layouts to consider.

We have run the layout algorithm for @hask«n» ranging from 1 to 15, and measured
the time to perform pretty-printing.
The following plot shows the time taken (in seconds) against
the @emph«number of lines of output». (Using the number of lines rather than @hask«n»
gives a more reasonable measure of the amount of work to perform for each layout task.)
The following plot shows the data on a double logarithmic scale
(note that several inputs can be printed on a single line):

@center(element performancePlotLog)
Precise timing were obtained by using O'Sullivan's @emph«criterion» benchmarking library.
Error bars are omitted because they are so thin that they
are not even visible at this scale.

The plot shows a behaviour that tends to become linear when the output is large enough.
For such large inputs approximately @(showFFloat (Just 2) regimeSpeed []) lines are laid out per second. We interpret this result as follows.
Our pretty-printer essentially considers non-dominated layouts. If the input is sufficiently complex, this approximately means to
consider one layout per possible width (@show(pageWidth) in our tests) --- when the width is given then the length and the width of last line are fixed.
Therefore, the amount of work becomes independent of the number of disjuctions present in the input,
and depends only on the amount of text to render.

@section«Discussion»

@subsection«Re-pairing with text»

Eventually, one might be interested in getting a complete pretty printed output, not just the amout of space that it takes. To do so we can pair measures with full-text layouts, while keeping the

@haskell«

instance Poset (M,L) where
  (a,_) ≺ (b,_) = a ≺ b

instance Layout (M,L) where
  (x,x') <> (y,y') =  (x<>y,x'<>y')
  flush (x,x') = (flush x, flush x')
  text s = (text s, text s)
  render = render . snd

instance Layout [(M,L)] where
  xs <> ys =  pareto $ concat
              [ filter (validMeasure . fst) [x <> y | y <- ys] | x <- xs]
  flush xs = pareto $ (map flush xs)
  text s = filter (validMeasure . fst) [text s]
  render = render . minimumBy (compare `on` fst)

»

@subsection«Hughes-Style nesting»

Hughes proposes a @hask«nest» conbinator, which indents its argument @emph«unless» it appears on the right-hand-side of a horizontal concatenation.
The above semantics are rather involved, and appear difficult to support by an incremental modification of the framework developed in this paper.

Fortunately, @hask«nest» appears to be used chiefly to implement the @hask«hang» combinator, which offers the choice between horizontal concatenation
and vertical concatenation with an indentation:
@haskell«
hang :: Doc d => Int -> d -> d -> d
hang n x y = (x <> y) <|> (x $$ nest n y)
»

In this context, nesting occurs on the right-hand-side of horizontal concatenation, and thus its semantics is simple; in fact
it can be implemented in terms of the combinators seen so far:

@haskell«
nest :: Layout d => Int -> d -> d
nest n y = spaces n <> y
  where spaces n = text (replicate n ' ')
»

@subsection«Ribbon length»

Another subtle feature of Hughes' library is the ability to limit
the amount of text on a single line, ignoring the current indentation.
The goal is to avoid long lines mixed with short lines.
While such a feature is easily added to Hughes or Wadler's greedy pretty printer,
it is harder to support as such on top of the basis we have so far.

What we would need to do is to record the length of the 1st line and length of the last line without indentation.
When concatenating, we add those numbers and check that they do not surpass the ribbon length. Unfortunately this
adds two dimensions to the search space, and renders the final algorithm impossibly slow.

An alternative is to interpret the ribbon length as the maximum size of a self-contained sublayout of one line.
Then we just filter out the intermediate results that do not fit the ribbon, as follows:

@haskell«
fitRibbon m = height m > 0 || maxWidth m < ribbonLength
  where ribbonLength = 60

valid m = validMeasure m && fitRibbon m
»

This re-interpretation appears to fulfil the original goal as well.

@subsection«Laws vs compositional semantics»

Note that laws may only @emph«partially» specify the behaviour, while a
semantic model will always fully constrain it.

(exercise: does the above set of laws fully constrain the semantic model?)

Notice that Hughes and Wadler give the semantics via laws first and
come up with a compositional interpretation second. This is fine,
precisely because laws do not fully constrain the design; there is
room for wiggle. However, a compositional semantics is often an even
better guide which should not be an afterthought.


@section«Conclusion»
Using three informal principles, we have defined what a pretty printer is.
We have carefully refined this informal definition to a formal semantics (arguably simpler than that of the state of the art), and
derived a reasonably efficient implementation. Along the way,
we have demonstrated how to use the standard functional programming methodology.

@acknowledgements«Using the QuickSpec tool, Nicholas Smallbone helped
finding a bug in the final implementation: the concatenation operator
did not preserve the invariant that lists were sorted. »


»

appendix = do
  cmd0"onecolumn"
  (cmd "section*" «Appendix»)
  subsection«Raw benchmark runtimes»
  performanceTable

  subsection«Proof details»
  paragraph«Proof of measure being a Layout-homomorphism»
  enumList [
   spec«
measure (a ++ [""])
                    == M { maxWidth = maximum ((map length) (a ++ [""]))
                         , height   = length  (a ++ [""]) - 1
                         , lastWidth = length $ last $ (a ++ [""])
                         }
                    == M { maxWidth  = maximum ((map length a) ++ [0])
                         , height    = length a + 1 - 1
                         , lastWidth = length ""
                         }
                    == M { maxWidth  = maximum (map length a)
                         , height    = length a - 1 + 1
                         , lastWidth = 0
                         }
                    == flush M { maxWidth  = maximum (map length a)
                                , height    = length a - 1
                                , lastWidth = length $ last $ a
                                }
                    == flush (measure a)
»
   ,
   spec«
measure (xs <> (y:ys))
                       == M { maxWidth = maximum ((map length) (init xs ++ [last xs ++ y] ++ map (indent ++) ys))
                            , height  = length (init xs ++ [last xs ++ y] ++ map (indent ++) ys) - 1
                            , lastWidth = length $ last $ (init xs ++ [last xs ++ y] ++ map (indent ++) ys)
                            }
                       == M { maxWidth = maximum ((init (map length xs) ++ [length (last xs) + length y] ++ map (\y -> length (last xs) + length y) ys))
                            , height  = length (init xs) + 1 + length ys - 1
                            , lastWidth = last $ ((init (map length xs) ++ [length (last xs) + length y] ++ map (\y -> length (last xs) + length y) ys))
                            }
                       == M { maxWidth = maximum (init (map length xs) ++ map (\y -> length (last xs) + length y) (y:ys))
                            , height  = (length xs - 1) + (length (y:ys) - 1)
                            , lastWidth = last $ (init (map length xs) ++ map (\y -> length (last xs) + length y) (y:ys))
                            }
                       == M { maxWidth = maximum [maximum (init (map length xs)), length (last xs) + maximum (map length (y:ys))]
                            , height  = (length xs - 1) + (length (y:ys) - 1)
                            , lastWidth = last $ (map (\y -> length (last xs) + length y) (y:ys))
                            }
                       == M { maxWidth = maximum [maximum (map length xs), length (last xs) + maximum (map length (y:ys))]
                            , height  = (length xs - 1) + (length (y:ys) - 1)
                            , lastWidth = length (last xs) + last $ (map length (y:ys))
                            }
                       == M { maxWidth = maximum (map length xs)
                            , height  = length xs - 1
                            , lastWidth = length (last xs)
                            } <>
                          M { maxWidth = maximum (map length (y:ys))
                            , height  = length (y:ys) - 1
                            , lastWidth = length (last (y:ys))
                            }
                      == measure xs <> measure (y:ys)
»

   ,spec«
measure (text s)
                 == M { maxWidth = maximum (map length [s])
                     , height = length [s] - 1
                      , lastWidth = length $ last $ [s]}
                 == M { maxWidth = length s
                      , height = 0
                      , lastWidth = length s}
                 == text s
»
   ]

improve_pareto_section :: TeX
improve_pareto_section = «
Unfortunately, computing the Pareto frontier as above is slow when
most elements are in the pareto frontier. Indeed, when adding an element
all the elements in the frontier so far a re-examined, and thus @hask«pareto»
has quadratic complexity.

There is a better way, which involves keeping the lists sorted in lexicographical
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
                          then     loop acc      xs
                          else x:  loop (x:acc)  xs
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
newtype D1 = D1 [M]

instance Doc D1 where
  D1 xs <|> D1 ys = D1 $ pareto $ merge xs ys
  fail = D1 []

instance Layout D1 where
  flush (D1 xs) = D1 $ map flush xs
»

In the implementation of the @hask«Layout» instance, we must
take care to keep lists sorted, in particular in the concatenation operation.
Let us propose the following implementation:

@haskell«
  D1 xs <> D1 ys = D1 $ pareto' $ mergeAll [ filter validMeasure [x <> y | y <- ys] | x <- xs]
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
  -- flush xs = pareto' $ mergeAll $ map sort $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = pareto' $ sort $ (map flush xs)
  text s = D1 [text s | valid (text s)]
  render (D1 (x:_)) = render x
»
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

@acknowledgements«Atze van der Ploeg gave useful feedback on a draft of this paper.
Using the QuickSpec tool, Nicholas Smallbone helped
finding a bug in the final implementation: the concatenation operator
did not preserve the invariant that lists were sorted. »

»



lineHeight = constant 6

abstrLayout :: Expr -> TexDiagram (Object,Point)
abstrLayout lastWidth = do
  bx <- noDraw $ box "abstrLayout"
  let points@[_nw,_ne,_se,after,_sse,_sw]
        = [bx # NW
          ,bx # NE
          ,bx # SE + Point zero lineHeight
          ,bx # SW + Point lastWidth lineHeight
          ,bx # SW + Point lastWidth zero
          ,bx # SW]
      p = polygon points
  path p
  return (Object "abstrLayout" p (anchors bx), after)


abstractLayoutJoin (a,a_last) (b,b_last) = do
  j <- boundingBox [a,b]
  b # NW .=. a_last
  return (j,b_last)

asse a = a # SW + Point zero lineHeight

lw1 = constant 12
lw2 = constant 18

showDot sz color p =
  using (outline color) $ do
    c <- circle "dot"
    c#Center .=. p
    width c === sz

dblarrow :: Box -> Point -> Point -> TexDiagram ()
dblarrow lab a b = do
   let points = [a,b]
       normal = OVector (avg points) (rotate90 (b-a))
       p = polyline points
   using (outline "black" . set endTip ToTip  . set startTip ToTip) $ path p
   tighten 10 $ autoLabelObj lab normal
   -- showDot 1 "black" (avg points)
   -- showDot 2 "green" (lab # Center)
   -- showDot 3 "red" (avg points + rotate90 (b-a))
   -- using (outline "blue") $ rectangleShape $ anchors lab
   return ()

hruler = gruler xpart ypart
vruler = gruler ypart xpart

gruler xp yp p1 p2 lab = do
  p1Obj <- point "p1'"
  p2Obj <- point "p2'"
  let [p1',p2'] = map (#Center) [p1Obj,p2Obj]
  l <- label "l" lab
  align xp [p1,p1']
  align xp [p2,p2']
  align yp [p1',p2']
  dblarrow l p1' p2'
  -- stroke "red" $
  boundingBox [p1Obj, p2Obj , l]

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
  a@(bx,_) <- draw $ abstrLayout (constant 40)
  width bx === constant 56
  D.height bx === 7 *- lineHeight
  rulersOfLayout «height» «maxWidth» «lastWidth» a
  return ()

twoLayouts :: TexDiagram ((Object,Point),(Object,Point))
twoLayouts = do
  (a,a_last) <- abstrLayout lw1
  (b,b_last) <- abstrLayout lw2
  width a === constant 48
  width b === constant 56
  D.height a === 4 *- lineHeight
  D.height b === 3 *- lineHeight

  return ((a,a_last),(b,b_last))


horizCat :: Bool -> TeX
horizCat showRulers = center $ element $ do
  (a,b) <- draw $ twoLayouts
  op <- label "<>" "<>"
  let lhsObjs = [fst a,op, fst b]
  align ypart $ [fst a # N, fst b # N]
  align ypart $ [fst a # Center, op # Center]

  eq <- label "=" "="
  abSep <- draw $ twoLayouts
  ab <- uncurry abstractLayoutJoin $ abSep
  (lhsObjsExtra,rhsObjsExtra) <- if showRulers
    then do
      (h1,_,lw1) <- rulersOfLayout «l1» «mw1» «lw1» a
      (_,_,_lw2) <- rulersOfLayout «l2» «mw2» «lw2» b

      (_hTot,mwTot,lwTot) <- rulersOfLayout «l1+l2» «max mw1 (lw1+mw2)» «lw1+lw2» ab
      spread hdist (constant 10) [h1,op,fst b]
      return ([lw1],[mwTot,lwTot])
    else do
      spread hdist (constant 10) lhsObjs
      return ([],[])
  spread hdist (constant 10) [eq, fst ab]
  align ypart $ map (#Center) [fst ab, eq]
  lhs <- boundingBox $ lhsObjs ++ lhsObjsExtra
  rhs <- boundingBox $ [eq, fst ab] ++ rhsObjsExtra
  align xpart $ map (#W) [rhs,lhs]
  spread vdist (constant 10) [rhs,lhs]

  return ()


spec = haskell

haskell_hidden :: Verbatim a -> TeX
haskell_hidden x = mempty

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
