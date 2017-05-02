{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies #-}

module PM where

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (deflike, mathpreamble,lemma,theorem,definition)
import MarXup.Tex hiding (label)
import qualified MarXup.Tex
import MarXup.Diagram hiding (height)
import qualified MarXup.Diagram as D
import MarXup.LineUp.Haskell
import MarXup.Verbatim
import MarXup.Latex.Math (ensureMath)
import Control.Lens (set)
import Data.Function
import Data.List (intercalate, minimumBy)
import Prelude hiding (fail,Num(..),(/),(^))
import Graphics.Diagrams.Plot
import System.IO.Unsafe (unsafePerformIO)
import Numeric (showFFloat, showEFloat)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Algebra.Classes
import Control.Monad (forM_)
import System.Random

newtype NoDom = NoDom [M]

randomDyck :: Int -> IO [OC]

instance Layout NoDom where
  NoDom xs <> NoDom ys =  NoDom (concat [ filter valid [x <> y | y <- ys] | x <- xs])
  flush (NoDom xs) = NoDom $ (map flush xs)
  text s = NoDom (filter valid [text s])
  render (NoDom xs) = render . minimum $ xs

instance Doc NoDom  where
  fail = NoDom []
  NoDom xs <|> NoDom ys = NoDom (xs ++ ys)

($$) :: forall l. Layout l => l -> l -> l
a $$ b = flush a <> b

data OC = Open | Close | A

tabular' :: [String] -> String -> [[TeX]] -> TeX
tabular' opts format bod = do
  usepkg "booktabs" 100 []
  env' "tabular" opts $ do
    braces (tex format)
    -- cmd0 "toprule"
    case (map mkcols bod) of
      (x:xs) -> do x ; newline; cmd0 "midrule"; mkrows xs
      [] -> return ()
  return ()

table :: TeX -> TeX -> Tex SortedLabel
table caption body = env "table" $ do
  cmd "caption" caption
  body
  MarXup.Tex.label "Table"


typicalPerfTable :: Tex SortedLabel
typicalPerfTable = table "Performance on typical JSON data" (tabular' [] "lrrr" typicalPerfData)

typicalPerfData :: [[TeX]]
typicalPerfData = [["Input", "Ours", "Wadler-Leijen", "Hughes-PJ"]
                   ,["JSON 1k", "9.7", "1.5", "3.0"]
                   ,["JSON 4k", "48.6", "6.3", "12.4"]
                   ,["JSON 10k", "145.5", "14.8", "30.0"]]


testExpr :: Int -> SExpr

newpage :: TeX
newpage = cmd0 "newpage"

instance Element String where
  type Target String = TeX
  element = textual

dataFileName :: FilePath
dataFileName = "benchmark-" ++ show pageWidth ++ ".dat"

performanceData :: String -> [(Integer, Integer, (Double,Double,Double))]
performanceData fname = unsafePerformIO $ do
  d <- readFile fname
  return [(i,h,(estLowerBound e, estPoint e, estUpperBound e)) | (i,h,e) <- read d]

regimeSpeed :: Double
regimeSpeed = fromIntegral nlines / time
  where ((_,nlines,(_,time,_)):_) = reverse (performanceData dataFileName)

performanceTable  :: String -> TeX
performanceTable fname = tabular [] "rrr" [[textual (show s), textual (show h),textual (show t)] | (s,h,t) <- performanceData fname]

performancePoints :: String -> [Point' Double]
performancePoints fname = [x | (_,x,_) <- performanceBars fname]

performanceBars :: String -> [(Point' Double,Point' Double,Point' Double)]
performanceBars fname = [(Point x l, Point x m, Point x h)
                  | (_,nlines,(l,m,h)) <- performanceData fname, let x = fromIntegral nlines]

scatterWithErrors :: PlotCanvas a -> [(Vec2 a,Vec2 a,Vec2 a)] -> TexDiagram ()
scatterWithErrors (bx,_outerBox,xform) inputs = do
  let three f (a,b,c) = (f a, f b, f c)
  forM_ (map (three (interpBox bx . (forward <$> xform <*>))) inputs) $ \(_l,m,_h) -> do
    -- draw $ path $ polyline [l,h]
    -- stroke "red" $ path $ polyline [m - Point 3 0, m + Point (-3) 0]
    showDot (constant 2) "black" m
    -- forM [l,h] $ \z -> stroke "red" $ path $ polyline [z - Point 2 0, z + Point 2 0]
    -- error bars are so narrow that we do not see them.

performancePlot :: String -> Vec2 (ShowFct TeX Double) -> Vec2 (Transform Double) -> Diagram TeX Tex (PlotCanvas Double)
performancePlot fname sho axes' =  do
  let  points = (performancePoints fname)
       points' = sequenceA points
  c@(bx,outerBox,_) <- preparePlot sho axes' (minimum <$> points') (maximum <$> points')
  scatterPlot c points
  width bx === constant 200
  D.height bx === constant 100
  xaxisLab <- label "xaxisLab" "number of lines of output"
  yaxisLab <- label "yaxisLab" "layout time (s)"
  align xpart $ map (#Center) [bx, xaxisLab]
  align ypart $ map (#Center) [bx, yaxisLab]
  hdist yaxisLab outerBox === constant 10
  outerBox `topOf` xaxisLab
  return c

renderFloat :: forall a. RealFloat a => a -> Tex ()
renderFloat x = tex $ showEFloat (Just 0) x ""

performancePlotLog, performancePlotLin :: String -> Diagram TeX Tex (PlotCanvas Double)
performancePlotLog fname = performancePlot fname (pure renderFloat) (Point (logAxis 10) (logAxis 10))
performancePlotLin fname = performancePlot fname (pure renderFloat) (Point (simplLinAxis 2000) (simplLinAxis 0.5))

performancePlotFull :: Diagram TeX Tex () 
performancePlotFull = do
  c <- performancePlotLog "benchmark-80.dat"
  functionPlot c 5 (\x -> x/regimeSpeed)
  return ()

performancePlotRandom :: Diagram TeX Tex () 
performancePlotRandom = do
  c <- performancePlotLog "benchmark-random.dat"
  functionPlot c 5 (\x -> x/(regimeSpeed * 10))
  return ()


tm :: Verbatim a -> Tex ()
tm x = do
  tex "$"
  tex $ fromVerbatim x
  tex "$"
  return ()

main :: IO ()
main = renderTex ACMArt "Prettiest" (preamble (header >> mainText >> bibliographyAll >> appendix))

comment :: TeX -> TeX
comment _ = mempty

preamble :: forall b. Tex b -> Tex b
preamble body = do
  documentClass "acmart" ["acmlarge"]
  cmd "setcitestyle" «authoryear»
  stdPreamble
  mathpreamble
  cmd "input" $ tex "../PaperTools/latex/unicodedefs"

  title "Functional Pearl: a pretty but not greedy printer"
  authorinfo [AuthorInfo "Jean-Philippe Bernardy" "jean-philippe.bernardy@gu.se" "University of Gothenburg, Department of Philosophy, Linguistics and Theory of Science"]
  env "document" body

principle :: TeX -> TeX -> Tex TeX
principle titl body = do
  _ <- deflike "Principle" "principle" "Principle" (smallcaps titl) body
  return $ smallcaps titl


header :: Tex ()
header = do
  abstract
  maketitle
  keywords $ ["Pearl", "Pretty Printing"]
  return ()

bibliographyAll :: TeX
bibliographyAll = do
  bibliographystyle "../PaperTools/bibtex/ACM-Reference-Format"
  bibliography  "../PaperTools/bibtex/jp"

abstract :: Tex ()
abstract = env "abstract" «
  This paper proposes a new specification of pretty printing which is stronger than the state of the art:
we require the output to be the shortest possible, and we also offer the ability to align sub-documents at will.
We argue that our specification precludes a greedy implementation. Yet,
we provide an implementation which behaves linearly in the size of the output.
The derivation of the implementation demonstrates
functional programming methodology.
»


mainText :: Tex ()
mainText = «


@sec_intro<-section«Introduction»



A pretty printer is a program that prints data structures in a way which
makes them pleasant to read. (The data structures in question
often represent programs, but not always.)
Pretty printing has historically been used by members of the functional programming
community to showcase proper style. Pro-eminent examples include, the pretty printer of @citet"hughes_design_1995",
which remains an influential example of functional programming design, and that of
@citet"wadler_prettier_2003" which was published as a chapter in a book dedicated to the @qu"fun of programming".

In addition to their aesthetic and pedagogical value, the pretty printers of Hughes and Wadler
are practical implementations. Indeed, they form the basis of industrial-strength pretty-printing packages which remain popular today. Hughes' design has been
refined by Peyton Jones, and is available as the
Hackage package @sans«pretty»@footnote«@url«https://hackage.haskell.org/package/pretty»»,
while Wadler's design has been extended by Leijen and made available as the
@sans«wl-print» package@footnote«@url«https://hackage.haskell.org/package/wl-pprint»». An ocaml implementation@footnote«@url«https://gallium.inria.fr/~fpottier/pprint/doc»» of Wadler's design also exists.

While this paper improves some aspects of the aforementioned landmark pieces of work in the functional programming landscape,
my goal is slightly different to that of Hughes and Wadler. Indeed, they aim first and foremost to
demonstrate general principles of functional programming development, with an emphasis on the efficiency of the algorithm.
Their methodological approach is to derive a @emph«greedy» algorithm from a functional specification.
In the process, they give themselves some leeway as to what they accept as pretty outputs (see @sec_notSoPretty).
In contrast, my primary goal is to produce @emph«the prettiest output», at the cost of efficiency. Yet, the final result is reasonably
efficient (@sec_timings).

@newpage
Let us specify the desired behavior of a pretty printer, first informally, as the following principles:

@pcp_visibility<-principle«Visibility»« A pretty printer shall
layout all its output within the width of the page.»

@pcp_layout<-principle«Legibility»« A pretty printer shall make appropriate use of layout, to make it easy
             for a human to recognize the hierarchical organization of data.»

@pcp_compact<-principle«Frugality»« A pretty printer shall minimize the number of lines
used to display the data.»

Furthermore, the first principle takes precedence over the second one, which itself takes precedence over the third one.
In the rest of the paper, we interpret the above three principles as an optimization problem, and derive a program
which solves it efficiently enough for practical purposes.

Before diving into the details, a couple of methodological points. 
First, Haskell is used throughout this paper in its quality the lingua franca of functional programming pearls.
Yet, we make no essential use of laziness. Second, 
the source code for the paper and benchmarks, as well as a fully fledged pretty printing library based on its principles is available
online: @url«https://github.com/jyp/prettiest». A Haskell library based on the algorithm developed here
can be found on Hackage: @url«https://hackage.haskell.org/package/pretty-compact».


@sec_api<-section«Interface (Syntax)»

Let us use an example to guide the development of our pretty-printing interface.
Assume that we want to pretty print S-Expressions, which can either be atom or a list of S-Expressions.
They can be represented in Haskell as follows:

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
three principles of pretty printing: @pcp_visibility, @pcp_layout and @pcp_compact.
While it is clear how the first two principles constrain the result, it
is less clear how the third principle plays out: we must specify more precisely which
layouts are admissible. To this end, we assert that in a pretty
display of an S-Expr, the elements should be either
concatenated horizontally, or aligned vertically. (Even though there possible choices, ours is sufficient for illustrative purposes.)
For example, the legible layouts of the @hask«abcd» S-Expression defined above would be either

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

And thus, @tex"{\\sc{}legibility}" will interact in non-trivial ways with @pcp_compact and @pcp_layout.

In general, a pretty printing library must provide the means to express
the set of legible layouts: it is up to the user to
instantiate @pcp_layout on the data structure of interest. The printer
will then automatically pick the smallest (@pcp_compact) legible layout which fits
the page (@pcp_visibility).

Our layout-description API
is similar to Hughes': we can express both
vertical (@hask«$$») and horizontal (@hask«<>») composition of
documents, as well as embed raw @hask«text» and provide
choice between layouts (@hask«<|>») --- but we lack a dedicated flexible space insertion operator (@hask«<+>»).
We give formal definition those in @sec_formal_semantics, but at this stage we keep
the implementation of documents abstract. We do so by using a typeclass (@hask«Doc») which
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
@hask«empty» document; horizontal concatenation with a fixed intermediate space
@hask«(<+>)»; vertical and horizontal concatenation of multiple
documents.

@haskell«
empty :: Layout d => d
empty = text ""

(<+>) :: Layout d => d -> d -> d
x <+> y  = x <> text " " <> y

hsep,vcat :: Doc d => [d] -> d
vcat  = foldDoc ($$)
hsep  = foldDoc (<+>)

foldDoc :: Doc d => (d -> d -> d) -> [d] -> d
foldDoc _ []      = empty
foldDoc _ [x]     = x
foldDoc f (x:xs)  = f x (foldDoc f xs)
»

We can furthermore define the choice between horizontal and
vertical concatenation:

@haskell«
sep :: Doc d => [d] -> d
sep []  = empty
sep xs  = hsep xs <|> vcat xs
»

Turning S-expressions into a @hask«Doc» is then straightforward:

@haskell«
pretty :: Doc d => SExpr -> d
pretty  (Atom s)    = text s
pretty  (SExpr xs)  =   text "(" <>
                        (sep $ map pretty xs) <>
                        text ")"
»

@sec_informal_semantics<-section«Semantics (informally)»

The above API provides a syntax to describe layouts. The next natural question is then: what should
its semantics be?  In other words, how do we turn the three principles into a
formal specification? In particular, how do we turn the above @hask«pretty» function into a pretty printer of S-Expressions?

Let us use an example to pose the question in concrete terms, and outline why neither Wadler's nor Hughes' answer is satisfactory for our purposes.
Suppose that we want
to pretty-print the following S-Expr (which is specially crafted to
demonstrate general shortcomings of both Hughes and Wadler libraries):

@haskell«
testData :: SExpr
testData = SExpr [  SExpr [Atom "abcde", abcd4],
                    SExpr [Atom "abcdefgh", abcd4]]
  where abcd4 = SExpr [abcd,abcd,abcd,abcd]
»

@fig_eighty<-figure_«
The expression @hask«testData» pretty-printed on 80 columns.»«
@verbatim«
12345678901234567890123456789012345678901234567890123456789012345678901234567890

((abcde ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
»»

Remember that by assumption we would like elements inside an S-Expr to be either
aligned vertically or concatenated horizontally (for
@pcp_layout), and that the second option should be preferred over the first
(for @pcp_compact), as long as the text fits within the page width
(for @pcp_visibility). More precisely, the three principles demand the output
with the smallest number of lines which still fits on the page among all the legible
outputs described above.
Thus on a 80-column-wide page, they demand the output
displayed in @fig_eighty and
on a 20-column-wide page, they demand the following output (the first line is not part of the output, but it helps by showing column numbers):
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
Yet, neither Hughes' nor Wadler's library can deliver those results.

@sec_notSoPretty<-subsection«The limitations of Hughes and Wadler»

Let us take a moment to see why.  On a 20-column
page and using Hughes' library, we would get the output shown in
@fig_hughes instead.
@fig_hughes<-figure«The expression @hask«testData» pretty-printed using Hughes' library.»«
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
»»
That output uses much more space than necessary, violating
@pcp_compact.  Why is that? Hughes states that @qu«it would be
unreasonably inefficient for a pretty-printer to decide whether or not
to split the first line of a document on the basis of the content of
the last.» (sec. 7.4 of his paper).  Therefore, he chooses a greedy
algorithm, which processes the input line by line, trying to fit as
much text as possible on the current line, without regard for what comes
next.  In our example, the algorithm can fit @teletype«(abcdefgh ((a»
on the sixth line, but then it has committed to a very deep
indentation level, which forces to display the remainder of the
document in a narrow area, wasting vertical space. Such a waste occurs in
many real examples: any optimistic fitting on an early line may waste
tremendous amount of space later on.

How does Wadler's library fare on the example? Unfortunately, we
cannot answer the question in a strict sense. Indeed, Wadler's API is
too restrictive to even @emph«express» the layout that we are after. That
is, one can only specify a @emph«constant» amount of indentation, not
one that depends on the contents of a document. In other words,
Wadler's library lacks the capability to express that a multi-line
sub-document @hask«b» should be laid out to the right of a document
@hask«a» (even if @hask«a» is single-line).  Instead, @hask«b» must be
put below @hask«a». Because of this restriction, even the best
pretty printer written using Wadler's library can only
produce the output shown in @fig_wadler:

@fig_wadler<-figure«The expression @hask«testData» pretty-printed using Wadler's library.»«
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
»

The above result does not look too bad --- but there is a spurious line break after the atom
@teletype«abcde». While Wadler's restriction may be acceptable to some, I find it
unsatisfying for two reasons. First, spurious line breaks may appear
in many places, so the rendering may be much longer than necessary, thereby violating @pcp_compact.
Second, and more importantly, a document which is laid out after another cannot
be properly indented in general. Suppose that we would like to
pretty print a ML-style equation composed of a @teletype«Pattern» and the following right-hand-side:
@verbatim«
expression [listElement x,
            listElement y,
            listElement z,
            listElement w]
»
Quite reasonably, we hope to obtain the following result, which puts the list to the
right of the @teletype«expression», clearly showing that the list is an argument of @teletype«expression», and thus properly respecting @pcp_layout:
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
respected. In summary, the lack of a combinator for relative indentation
is a serious drawback. In fact, Leijen's
implementation of Wadler's design (@sans«wl-print»), @emph«does» feature
an alignment combinator. However, as Hughes' does, Leijen's uses a greedy algorithm, and thus
suffers from the same issue as Hughes' library.

In summary, we have to make a choice between either respecting the three principles
of pretty printing, or providing a greedy algorithm. Hughes does not
fully respect @pcp_compact. Wadler does not fully respect
@pcp_layout. Here, I decide to respect both, but I give up on
greediness.
Yet, the final algorithm that I arrive at is fast enough for
common pretty-printing tasks.

But let us not get carried away. Before attacking the problem of making an implementation,
we need to finish the formalization of the semantics. And before that,
it is best if we spend a moment to further refine the API for defining pretty layouts.

@sec_formal_semantics<-section«Semantics (formally)»

@subsection«Layouts»
We ignore for a moment the choice between possible layouts
(@hask«<|>»). As Hughes does, we call a document without choice a @emph«layout».

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
Hughes, I shall simply use the type of lists, trusting the reader to remember the invariant of non-emptiness.

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
where @hask«intercalate» can be defined as follows:
@spec«
  intercalate :: String -> [String] -> String
  intercalate x []      = []
  intercalate x (y:ys)  = y ++ x ++ intercalate ys
»


Embedding a string is thus immediate:
@haskell«
  text :: String -> L
  text s = [s]
»
The interpretation of vertical concatenation ($$) requires barely more
thought: it suffices to concatenate the input lists.
@spec«
  ($$) :: L -> L -> L
  xs $$ ys = xs ++ ys
»
The only potential difficulty is to figure out the interpretation of
horizontal concatenation (@hask«<>»). We follow the advice provided by @citet"hughes_design_1995":
@qu«translate the second operand [to the right], so that its first character abuts against
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

Or, diagrammatically:

@(horizCat False)

Algorithmically, one must handle the last line of the first layout and the
first line of the second layout specially, as follows:

@haskell«
  (<>) :: L -> L -> L
  xs <> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
     where  xs0 = init xs
            x :: String
            x = last xs
            n = length x
            indent = replicate n ' '
»

We take a quick detour to refine our API a bit.
Indeed, as becomes clear with the above definition, vertical concatenation is (nearly)
a special case of horizontal composition. That is,
instead of composing vertically, one can add an empty line to the
left-hand-side layout and then compose horizontally. The combinator which adds
an empty line is called @hask«flush», and has the following definition:
@haskell«
  flush :: L -> L
  flush xs = xs ++ [""]
»
Vertical concatenation is then:
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
laws, (written here as QuickCheck properties@footnote«These properties can (and were) checked when properly monomorphized using
either of the concrete implementations provided later. The same applies for all properties stated in the paper.»):

@enumList[
«Layouts form a monoid, with operator (@hask«<>») and unit @hask«empty»@footnote«recall @hask«empty = text ""»»:

@haskell«
propLeftUnit :: (Doc a, Eq a) => a -> Bool
propLeftUnit a = empty <> a == a
»
@vspace"1ex"
@haskell«
propRightUnit :: (Doc a, Eq a) => a -> Bool
propRightUnit a = a <> empty == a
»
@vspace"1ex"
@haskell«
propAssoc :: (Doc a, Eq a) => a -> a -> a -> Bool
propAssoc a b c = (a <> b) <> c == a <> (b <> c)
»»
,
«@hask«text» is a monoid homomorphism:
@spec«
propTextAppend s t  = text s <> text t == text (s ++ t)
propTextEmpty       = empty == text ""
»
»,
« @hask«flush» can be pulled out of concatenation, in this way:

@haskell«
propFlush :: (Doc a, Eq a) => a -> a -> Bool
propFlush a b =  flush a <> flush b == flush (flush a <> b)
»
One might expect this law to hold instead:
@spec«flush a <> flush b == flush (a <> b)»
However, the inner @hask«flush» on @hask«b» goes back to the local indentation level, while the outer @hask«flush» goes back to the outer indentation level, which are equal only if @hask«a» ends with an empty line. In turn this condition is guaranteed only when @hask«a» is itself flushed on the right-hand side.

»]


@subsection«Choice»

We proceed to extend the API with choice between layouts, yielding the
final API to specify legible documents. The extended API is accessible via a
new type class:

@haskell«
class Layout d => Doc d where
  (<|>) :: d -> d -> d
  fail :: d
»

Again, we give the compositional semantics straight away. Documents are
interpreted as a set of layouts. We implement sets as lists, and we will
take care not to depend on the order and number of occurrences.

The interpretation of disjunction merely appends the list of possible layouts:
@haskell«
instance Doc [L] where
  xs <|> ys = (xs ++ ys)
  fail = []
»

Consequently, disjunction is associative.

@haskell«

propDisjAssoc :: (Doc a, Eq a) => a -> a -> a -> Bool
propDisjAssoc a b c = (a <|> b) <|> c == a <|> (b <|> c)
»
@comment«propLeftUnit' :: (Doc a, Eq a) => a -> Bool
propLeftUnit' a = fail <|> a == a

propRightUnit' :: (Doc a, Eq a) => a -> Bool
propRightUnit' a = a <|> fail == a
»

We simply lift the layout operators idiomatically @citep"mcbride_applicative_2007" over sets:
elements in sets are treated combinatorially.

@haskell«
instance Layout [L] where
  text = pure . text
  flush = fmap flush
  xs <> ys = (<>) <$> xs <*> ys
»

Consequently, concatenation and @hask«flush» distribute over disjunction:

@spec«
propDistrL :: (Doc a, Eq a) => a -> Bool
propDistrL a = (a <|> b) <> c == (a <> c) <|> (b <> c)

propDistrR :: (Doc a, Eq a) => a -> Bool
propDistrR a = c <> (a <|> b) == (c <> a) <|> (c <> b)

propDistrFlush :: (Doc a, Eq a) => a -> a -> Bool
propDistrFlush a b = flush (a <|> b) == flush a <|> flush b
»

@subsection«Semantics»
We can finally define formally what it means to render a document.
We wrote above that prettiest layout is that the solution of the optimization problem given
by combining all three principles. Namely, to pick a most frugal layout among the visible ones:
@haskell«
  render =   render .  -- (for layouts)
             mostFrugal .
             filter visible
»
Note that the call to @hask«render» in the above is for the @hask«L» instance.
The rest of the above definition breaks down as follows.
@pcp_visibility is formalized by the @hask«visible» function, which states that all lines must fit on the page:
@haskell«
visible :: L -> Bool
visible xs = maximum (map length xs) <= pageWidth

pageWidth = 80
»
@pcp_compact is formalized by the @hask«mostFrugal» function, which picks a layout with the least number of lines:
@haskell«
mostFrugal :: [L] -> L
mostFrugal = minimumBy size
  where size = compare `on` length
»
@pcp_layout is realized by the applications-specific set of layouts, specified by the API of @sec_api, which
comes as an input to @hask«render».


One may expect that disjunction should also be commutative.
However, the implementation of @hask«mostFrugal» only picks @emph«one» of
the most frugal layouts. That is fine, because all most frugal layouts are
equally good. However it also means that re-ordering the arguments of a disjunction may
affect the layout being picked. Therefore, commutativity of disjunction holds
only up to the length of the layout being rendered:

@haskell«
propDisjCommut :: Doc a => a -> a -> Bool
propDisjCommut a b = a <|> b =~ b <|> a

infix 3 =~
(=~) :: Layout a => a -> a -> Bool
(=~) = (==) `on` (length . lines . render)
»

We have now defined semantics compositionally. Furthermore, this semantics is executable.
Consequently, we can implement the pretty printing an S-Expr as follows:

@haskell«
showSExpr x = render (pretty x :: [L])
»

Running @hask«showSExpr» on our example (@hask«testData») may eventually yield the output that we demanded in @sec_informal_semantics.
But one should not expect to see it any time soon.
Indeed, while the above semantics provides an executable implementation, it is impracticably slow.
Indeed, every possible combination of choices is first constructed, and only then a shortest output is
picked. Thus, for an input with @ensureMath«n» choices, the running time is @tm«O(2^n)».


@sec_optimization<-section«A More Efficient Implementation»
The last chunk of work is to transform above, clearly correct but inefficient implementation
to a functionally equivalent, but efficient one.
We do so we need two insights.

@subsection«Measures»

The first insight is that it is
not necessary to fully construct layouts to calculate their size: only some of their parameters are relevant.
Let us remember that we want to sift through layouts based on the space that they take.
Hence, from an algorithmic point of view, all that matters is a measure of that space.
Let us define an abstract semantics for
layouts, which ignores the text, and captures only the amount of space used.

The only parameters that matter are the maximum width of the layout, the width of its
last line and its height (and, because layouts cannot be empty and it is convenient to counting start at zero, we do not
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

The above diagram can be read out as Haskell code:
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

The correctness of the @hask«Layout M» instance relies on intuition, and a
proper reading of the concatenation diagram. This process being
informal, we must cross-check the final result formally.
To do so, we define a function which computes the measure of a full layout:
@haskell«
measure :: L -> M
measure xs = M {  maxWidth   = maximum $ map length $ xs,
                  height     = length xs - 1,
                  lastWidth  = length $ last $ xs}
»

Then, to check the correctness of the @hask«Layout M» instance, we
verify that @hask«measure» is a layout homomorphism (ignoring of
course the @hask«render»er). This homomorphism property can be spelled out as the following
three laws:

@lemma«Measure is a Layout-homomorphism»«
@spec«
measure (a <> b)   == measure a <> measure b
measure (flush a)  == flush (measure a)
measure (text s)   == text s
»
(Note: on the left-hand-side of the above equations,
the combinators (@hask«<>», @hask«flush», @hask«text») come from
the @hask«L» instance of @hask«Layout», while on the right-hand-side they come from the @hask«M» instance.)

»«
Checking the laws is a simple, if somewhat tedious exercise of program calculation, and thus it is deferred to the appendix.
»

Using the measure, we can check that a layout is fully visible simply by checking that @hask«maxWidth» is small enough:
@haskell«
valid :: M -> Bool
valid x = maxWidth x <= pageWidth
»

Having properly refined the problem, and continuing to ignore the detail of
actually rendering the text, we may proceed to give a fast
implementation of the pretty printer.

@subsection«Early filtering out invalid results»

The first optimization is to filter out invalid results early; like so:

@spec«
text x = filter valid [text x]
xs <> ys = filter valid [x <> y | x <- xs, y <- ys]
»

We can do so because de-construction preserves @hask«valid»ity:
the validity of a document implies the validity of its part.

@lem_valid_mono<-lemma«de-construction preserves validity.»«

The following two implications hold:
@spec«
valid (a <> b)   => valid a  ∧  valid b
valid (flush a)  => valid a
»»«
We prove the two parts separately:
@enumList[
spec«
    valid (a <> b)
=>  maxWidth (a <> b) < pageWidth
=>  max (maxWidth a) (lastWidth a +  maxWidth b) <= pageWidth
=>  maxWidth a < pageWidth  ∧ lastWidth a +  maxWidth b <= pageWidth
=>  maxWidth a < pageWidth  ∧ maxWidth b <= pageWidth
=>  valid a  ∧  valid b
  »,
spec«


valid (flush a)  => maxWidth (flush a) <= pageWidth
                 => maxWidth a <= pageWidth
                 => valid a
  »]
  »
Consequently, keeping invalid layouts is useless: they can never be
combined with another layout to produce something valid.

@lemma«Invalid layouts cannot be fixed»«
@spec«
not (valid a)    => not (valid (a <> b))
not (valid b)    => not (valid (a <> b))
not (valid a)    => not (valid (flush a))
»»«By contrapositive of @lem_valid_mono»

@subsection«Pruning out dominated results»

The second optimization relies on the insight that even certain valid results
are dominated by others. That is, they can be discarded early.

We write @hask«a ≺ b» when @hask«a» dominates @hask«b». We will arrange
our domination relation such that
@enumList[«Layout operators are monotonic with respect to domination.
           Consequently, for any document context
           @hask«ctx :: Doc d => d -> d»,

          if @hask«a ≺ b» then @hask«ctx a ≺ ctx b»»
          ,«If @hask«a ≺ b», then @hask«a» is at least as frugal as @hask«b».»]

Together, these properties mean that we can always discard dominated
layouts from a set, as we could discard invalid ones. Indeed, we have:
@theorem«(Domination)»« For any context @hask«ctx», we have
@spec«a ≺  b  =>  height (ctx a) <= height (ctx b)»
»«By composition of the properties 1. and 2.»

We can concretize the above abstract result by
defining our domination relation and proving its properties 1. and 2.
Our domination relation is a partial order
(a reflexive, transitive and antisymmetric relation), and thus we can make it an instance
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

@lemma«@hask«flush» is monotonic.»«


  if @spec«      m1 ≺  m2 » then   @spec«flush m1 ≺ flush m2 »»«
By definition, the assumption expands to
@spec«
height     m1 <= height     m2
maxWidth   m1 <= maxWidth   m2
lastWidth  m1 <= lastWidth  m2
»
similarly, the conclusion that we aim to prove expands to the following three conditions

@spec«
height     (flush m1) <= height     (flush m2)
maxWidth   (flush m1) <= maxWidth   (flush m2)
lastWidth  (flush m1) <= lastWidth  (flush m2)
»

by definition, they respectively reduce to the following inequalities, which are easy consequences of the assumptions.

@spec«
height     m1 + 1  <= height     m2 + 1
maxWidth   m1      <= maxWidth m2
0                  <= 0
»


»

@lemma«concatenation is monotonic»«
@spec«if    m1 ≺  m2 and  m'1 ≺  m'2   => (m1 <> m'1) ≺  (m2 <> m'2)  »

»«
Each of the assumptions expand to three conditions. Thus we have:
@spec«
height     m1 <= height     m2
maxWidth   m1 <= maxWidth   m2
lastWidth  m1 <= lastWidth  m2
height     m'1 <= height     m'2
maxWidth   m'1 <= maxWidth   m'2
lastWidth  m'1 <= lastWidth  m'2
»
and similarly we need to prove the following three conditions to obtain the conclusion:
@spec«
height     (m1 <> m'1) <= height     (m2 <> m'2)
maxWidth   (m1 <> m'1) <= maxWidth   (m2 <> m'2)
lastWidth  (m1 <> m'1) <= lastWidth  (m2 <> m'2)
»
These are respectively equivalent to the following ones, by definition:
@spec«
height m1 + height m'1 <= height     m2 + height m'2

max (maxWidth m1) (lastWidth m1 + maxWidth m'1)
  <= max (maxWidth m2) (lastWidth m2 + maxWidth m'2)

lastWidth  m1 + lastWidth m'1 <= lastWidth  m2 + lastWidth m'2
»

The first and third inequalities are consequences of the assumptions combined with the monotonicity of @hask«+».
The second inequation can be obtained likewise, with additionally using the monotonicity of @hask«max»:
@spec«
a <= b ∧ c <= d   =>  max a c <= max b d
»

»



@subsection«Pareto frontier»
The subset of non-dominated elements is known as the Pareto frontier @citep"deb_multi_2016".
@definition«Pareto frontier»«
@tm«\mathnormal{Pareto}(X) = \{ x∈X | ¬∃y∈X. x ≠ y ∧ y ≺ x\}»
»
When sets are represented as lists without duplicates, the Pareto frontier can be computed as follows.
@haskell«
pareto :: Poset a => [a]  -> [a]
pareto = loop []
  where  loop acc  []      = acc
         loop acc  (x:xs)  =
            if any (≺ x) acc
               then  loop acc xs
               else  loop (x:filter (not . (x ≺)) acc) xs
»

The above @hask«loop» function examines elements sequentially, and keeps a Pareto frontier
of the elements seen so far in the @hask«acc» parameter. For each examined element @hask«x», if it
is dominated, then we merely skip it.  Otherwise, @hask«x» is added to
the current frontier, and all the elements dominated
by @hask«x» are then removed.

The implementation of the pretty-printing combinators then becomes:

@haskell«
type DM = [M]

instance Layout DM where
  xs <> ys =  pareto $ concat
              [ filter valid [x <> y | y <- ys] | x <- xs]
  flush xs = pareto $ (map flush xs)
  text s = filter valid [text s]
  render = render . minimum

instance Doc DM where
  fail = []
  xs <|> ys = pareto (xs ++ ys)
»

The above is the final, optimized version of the layout-computation algorithm.

@section«Additional features»
To obtain a complete library from the above design,
one should pay attention to a few more points that we discuss in this section.

@subsection«Re-pairing with text»

Eventually, one might be interested in getting a complete
pretty printed output, not just the amount of space that
it takes. To do so we can pair measures with full-text
layouts, while keeping the measure of space for actual computations:

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
              [ filter (valid . fst) [x <> y | y <- ys] | x <- xs]
  flush xs = pareto $ (map flush xs)
  text s = filter (valid . fst) [text s]
  render = render . minimumBy (compare `on` fst)

instance Doc [(M,L)] where
  fail = []
  xs <|> ys = pareto (xs ++ ys)
»

@subsection«Hughes-Style nesting»

Hughes proposes a @hask«nest» combinator, which indents its argument @emph«unless» it appears on the right-hand-side of a horizontal concatenation.
The above semantics are rather involved, and appear difficult to support by a local modification of the framework developed in this paper.

Fortunately, in practice @hask«nest» is used only to implement the @hask«hang» combinator, which offers the choice between horizontal concatenation
and vertical concatenation with an indentation:
@haskell«
hang :: Doc d => Int -> d -> d -> d
hang n x y = (x <> y) <|> (x $$ nest n y)
»

In this context, nesting occurs on the right-hand-side of vertical concatenation, and thus its semantics is much simpler. In fact,
in the context of @hask«hang»,
it can be implemented easily in terms of the combinators provided so far:

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
algorithm adds two dimensions to the search space, and slows the final algorithm to impractical speeds.

An alternative approach to avoid too long lines is to interpret the ribbon length as the maximum
size of a self-contained sublayout fitting on a single line. This interpretation can
be implemented simply, by filtering out intermediate results that do not fit the ribbon.
This can be done be re-defining @hask«valid» as follows:

@haskell«
fitRibbon m = height m > 0 || maxWidth m < ribbonLength
  where ribbonLength = 60

valid' m = valid m && fitRibbon m
»

This re-interpretation appears to fulfill the original goal as well.

@sec_timings<-section«Performance tests»

Having optimized our algorithm as best we could, we turn to empirical test to
evaluate its performance.
Our benchmarking tool is O'Sullivan's @emph«criterion» benchmarking library,
which provides precise timings even for operations lasting less than a microsecond.
All benchmarks we run on an Intel Xeon E5-2640 v4 (running on a single core), using GHC 8.0.

@subsection«Behaviour at scale»

In order to benchmark our pretty printer on large outputs, we have used it to lay out
full binary trees and random trees, represented as S-Expressions.
The set of layouts were computed using the pretty printer for S-Expressions
shown above. The most efficient version of the pretty-printer (shown at the end of @sec_optimization) was used.
Then we then measured the time to compute the length of the best layout.
Indeed, computing the length is enough to force the computation of the best layout.
The results are displayed in plots which uses a double logarithmic scale and show the time taken against
the @emph«number of lines of output». By using the number of lines (rather than, say, the depth of the tree),
we have a more reasonable measure of the amount of work to perform for each layout task.

@paragraph«Full trees»

S-expressions representing full binary trees of increasing depth were generated by the following function:

@haskell«
testExpr 0 = Atom "a"
testExpr n = SExpr [testExpr (n-1),testExpr (n-1)]
»

Pretty-printing the generated S-expression heavily exercises the disjunction construct.
Indeed, for each S-Expressions with two
sub-expressions, the printer introduces a choice, therefore the number of choices is equal to the number of nodes in a
binary tree of depth @hask«n».
Thus, for @hask«testExpr n» the pretty printer is offered @tm«2^n-1» choices,
for a total of @tm«2^{2^n-1}» possible layouts to consider.

We have run the layout algorithm for @hask«n» ranging from 1 to 16, and obtained the following results.

@center«@performancePlotFull»
While @emph«criterion» provides confidence intervals, they are so thin that they
are not visible at the scale of the plots, thus we have not attempted to render them.
We observe that inputs for @hask«n»@tm«∈[0,1,2,3]» can all be printed on a single line,
and thus these data points should be dismissed.

Otherwise, the plot shows a behavior that tends to become linear when the output is large enough.

For such large inputs approximately @(showFFloat (Just 2) regimeSpeed []) lines are laid out per second.
(Data points exhibiting this speed lay on the straight line which we overlaid to the plot.)

We interpret this result as follows.
Our pretty-printer essentially considers non-dominated layouts. If the input is sufficiently complex, this means to
consider approximately one layout per possible width (@show(pageWidth) in our tests) --- when the width is given then the length and the width of last line are fixed.
Therefore, for sufficiently large outputs the amount of work becomes independent of the number of disjunctions present in the input,
and depends only on the amount of text to render.

@paragraph«Random trees»

One may wonder if the effect that we observe is not specific to full trees.
To control this hypothesis we ran the same experiment on 100 random S-expressions of exponentially increasing length.
These S-expressions were generated by picking random Dyck words of a certain length (using the @hask«randomDyck» function shown below) and then interleaving parentheses
with atoms.
@haskell«
randomDyck maxLen = go 0 0 where
    go opened closed
      | closed >= maxLen = return []
      | opened >= maxLen = close
      | closed >= opened = open
      | otherwise = do
          b <- randomIO
          if b then open else close
      where  open  = (Open: ) <$> go (1+opened) closed
             close = (Close:) <$> go opened (1+closed)
»
We obtained to following results:
@center«@performancePlotRandom»
The results corroborate those obtained for full trees:
the observed running time is proportional to the length of the output.
Furthermore the layout speed for random trees is roughly 10 times that of full trees;
the straight line corresponding to this speed is shown for reference on the plot.

A reviewer how the plots would look like if the elimination of dominated outputs was not activated.
Unfortunately we could not produce more than four useful data points: the exponential behaviour of
the algorithm meant that our test machine ran out of memory even for relatively simple outputs.
Because an asymptotic behaviour can hardly be derived from so few points,
we chose to omit the corresponding plot.
@subsection«Tests for full outputs and typical inputs»

Even though the asymptotic behaviour is linear, one may wonder if the constant factor is
satisfactory for typical pretty-printing tasks. Thus we evaluated the performance of a
complete pretty-printing task, including
not only the selection of the layout but its actual printing. We did so using our complete
library@footnote«@url«https://hackage.haskell.org/package/pretty-compact»». For reference, we performed the same
tests using the Wadler-Leijen library and the Hughes-Peyton Jones library. The inputs were JSON
files of 1k, 4k and 40k lines generated by the tool found at @url«http://www.json-generator.com/», which
aims to generate typical JSON files. The results are displayed in @tbl_perf.
@tbl_perf<-typicalPerfTable
We observe that our library is capable of outputting roughly 70.000 lines of pretty-printed JSON
per second, which is acceptable for many applications. This result makes our library
roughly ten times as slow as that of Wadler-Leijen, and five times as slow as that of Hughes-Peyton Jones.


@section«Conclusion»

As @citet"bird_algebra_1997", @citet"wadler_critique_1987", @citet"hughes_design_1995" and many
others have argued, program calculation is a useful tool, and a strength of functional programming languages,
with a large body of work showcasing it. Nevertheless, the problem of pretty-printing had not been
contrived to fit the mold of program calculation, before becoming one of its paradigmatic applications.
And thus, one could wonder if program calculation was only well-suited to derive greedy algorithm.
We hope to have put such doubts to rest.

Indeed, we have taken a critical look at the literature to re-define what pretty-printing means, as
three informal principles.
We have carefully refined this informal definition to a formal semantics (arguably simpler than that of the state of the art).
We avoided cutting any corner, and thus could not obtain a greedy algorithm, but we still have
derived a reasonably efficient implementation.
In the end, the standard methodology worked well: we could use program calculation all the way.

@acknowledgements«Most of the work described in this paper was carried out while the author was employed by Chalmers University of Technology.
Facundo Domingez, Atze van der Ploeg and Arnaud Spiwack as well as anonymous ICFP reviewers provided useful feedback on drafts of this paper.
Using the QuickSpec tool, Nicholas Smallbone helped
finding a bug in the final implementation: the concatenation operator
did not preserve the invariant that lists were sorted. »
»


appendix :: Tex ()
appendix = do
  cmd0"onecolumn"
  (cmd "section*" «Appendix»)
  -- subsection«Raw benchmark runtimes»
  -- performanceTable dataFileName

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
                       == M { maxWidth = maximum (( init (map length xs) ++ [length (last xs) + length y] ++
                                                    map (\y -> length (last xs) + length y) ys))
                            , height  = length (init xs) + 1 + length ys - 1
                            , lastWidth = last ((  init (map length xs) ++ [length (last xs) + length y] ++
                                                   map (\y -> length (last xs) + length y) ys))
                            }
                       == M { maxWidth = maximum (init (map length xs) ++ map (\y -> length (last xs) + length y) (y:ys))
                            , height  = (length xs - 1) + (length (y:ys) - 1)
                            , lastWidth = last $ (init (map length xs) ++ map (\y -> length (last xs) + length y) (y:ys))
                            }
                       == M { maxWidth = maximum [  maximum (init (map length xs)),
                                                    length (last xs) + maximum (map length (y:ys))]
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
order. Then the pareto frontier has a more efficient implementation.

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
In summary, so no element @hask«m0» already in the frontier can be dominated by @hask«m1».

Consequently, on a sorted list, we can skip the re-filtering of the @hask«acc»umulated frontier, as follows:

@haskell«
pareto' :: Poset a => [a] -> [a]
pareto' = loop [] where
  loop acc  []      = []
  loop acc  (x:xs)  = if any (≺ x) acc
                          then     loop acc      xs
                          else x:  loop (x:acc)  xs
»

In order to make use of the optimized Pareto frontier algorithm, we must then ensure that the operators preserve the lexicographically
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
  D1 xs <> D1 ys = D1 $ pareto' $ mergeAll [ filter valid [x <> y | y <- ys] | x <- xs]
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


»



lineHeight :: Expr
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


abstractLayoutJoin :: (Object, Point) -> (Object, t) -> TexDiagram (Object, t)
abstractLayoutJoin (a,a_last) (b,b_last) = do
  j <- boundingBox [a,b]
  b # NW .=. a_last
  return (j,b_last)

asse :: Object -> Point
asse a = a # SW + Point zero lineHeight

lw1, lw2 :: Expr
lw1 = constant 12
lw2 = constant 18

showDot :: Expr -> Color -> Point -> TexDiagram ()
showDot sz colour p =
  using (outline colour) $ do
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

hruler :: Point -> Point -> TeX -> Diagram TeX Tex Object
hruler = gruler xpart ypart
vruler :: Point -> Point -> TeX -> Diagram TeX Tex Object
vruler = gruler ypart xpart

gruler :: (Point -> Expr)
                -> (Point -> Expr)
                -> Point
                -> Point
                -> TeX
                -> Diagram TeX Tex Object
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

rulersOfLayout :: forall a a1 a2.
                        Verbatim a
                        -> Verbatim a1
                        -> Verbatim a2
                        -> (Object, Point)
                        -> Diagram TeX Tex (Object, Object, Object)
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
  _ <- rulersOfLayout «height» «maxWidth» «lastWidth» a
  return ()

twoLayouts :: TexDiagram ((Object,Point),(Object,Point))
twoLayouts = do
  (a,a_last) <- using (fill "lightgray") (abstrLayout lw1)
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
      (h1, mw1,lw1Rule) <- rulersOfLayout «l_a» «mw_a» «lw_a» a
      (h2,_mw2,lw2Rule) <- rulersOfLayout «l_b» «mw_b» «lw_b» b

      (hTot,mwTot,lwTot) <- rulersOfLayout «l_a+l_b» «max mw_a (lw_a+mw_b)» «lw_a+lw_b» ab
      spread hdist (constant 7) [h1,op,fst b]
      return ([lw1Rule,mw1,h2],[mwTot,lwTot,hTot])
    else do
      spread hdist (constant 7) lhsObjs
      return ([],[])
  spread hdist (constant 15) [eq, fst ab]
  align ypart $ map (#Center) [fst ab, eq]
  lhs <- boundingBox $ lhsObjs ++ lhsObjsExtra
  rhs <- boundingBox $ [eq, fst ab] ++ rhsObjsExtra
  align ypart $ map (#W) [rhs,lhs]
  spread hdist (constant 10) [lhs,rhs]

  return ()


spec :: forall a. Verbatim a -> Tex ()
spec = haskell

haskell_hidden :: Verbatim a -> TeX
haskell_hidden _ = mempty

verbatim :: Verbatim () -> TeX
verbatim (Verbatim s _) =
    env "verbatim" (tex s)

-- hask = ensureMath . cmd "mathsf"
hask :: Verbatim a -> Tex ()
hask = ensureMath . haskellInline


url :: Verbatim a -> TeX
url (Verbatim s _) = do
  usepkg "url" 100 []
  cmd "url" (tex s)

displayLeft :: Tex a -> Tex a
displayLeft body = env'' "list" [] [mempty,tex "\\setlength\\leftmargin{1em}"] $ do
  texLn "\\item\\relax"
  body

display :: Tex a -> Tex a
display = env "center"

footnote :: forall a. Tex a -> Tex a
footnote = cmd "footnote"

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-target: "paper"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal --sandbox-config-file=../cabal.sandbox.config repl --only paper")
-- End:

--  LocalWords:  XTypeSynonymInstances XOverloadedStrings pgmF marxup
--  LocalWords:  XRecursiveDo FlexibleInstances InstanceSigs MarXup
--  LocalWords:  GeneralizedNewtypeDeriving FlexibleContexts deflike
--  LocalWords:  RankNTypes TypeFamilies mathpreamble ensureMath forM
--  LocalWords:  intercalate minimumBy unsafePerformIO showFFloat nf
--  LocalWords:  showEFloat DataRecord SampleAnalysis getArgs Num cmd
--  LocalWords:  runAndAnalyseOne forall newpage Eq Benchmarkable DM
--  LocalWords:  testOne testExpr SExpr dataFileName FilePath dat sz
--  LocalWords:  pageWidth performanceData readFile estLowerBound dt
--  LocalWords:  estPoint estUpperBound measTime regimeSpeed nlines
--  LocalWords:  fromIntegral putStrLn withConfig Wno newtype NoDom
--  LocalWords:  defaultConfig liftIO Analysed reportAnalysis anMean
--  LocalWords:  writeFile performanceTable rrr performancePoints sho
--  LocalWords:  performanceBars scatterWithErrors PlotCanvas showDot
--  LocalWords:  TexDiagram interpBox polyline performancePlot tex tm
--  LocalWords:  ShowFct sequenceA preparePlot functionPlot RealFloat
--  LocalWords:  renderFloat performancePlotLog performancePlotLin qu
--  LocalWords:  logAxis simplLinAxis ErrorBar lbound ubound args env
--  LocalWords:  fromVerbatim renderTex ACMArt mainText mempty acmart
--  LocalWords:  bibliographyAll documentClass acmlarge stdPreamble
--  LocalWords:  authorinfo AuthorInfo titl smallcaps maketitle wl xs
--  LocalWords:  bibliographystyle abbrvnat hughes Peyton Wadler's ys
--  LocalWords:  Leijen ocaml notSoPretty emph pcp api Expr abcd hask
--  LocalWords:  ing hsep vcat foldr sep testData abcde abcdefgh Daan
--  LocalWords:  listElement Leijen's compositional xxxxxxxxxxxxx Ord
--  LocalWords:  yyyyyyyyyyyyyyyyyyyyy xxxxxxxxx xxxxxxxxxxxx yyyy OC
--  LocalWords:  yyyyyyyyyyyyyyyyyyyyyyyyyyyyy xxxxxx horizCat init
--  LocalWords:  xxxxxxyyyyyyyyyyyyyyyyyyyyy Algorithmically enumList
--  LocalWords:  leftUnit rightUnit disj citep mcbride applicative PJ
--  LocalWords:  fmap distrl distrr distrflush mostFrugal TODO commut
--  LocalWords:  compositionally showSExpr singleLayoutDiag lastWidth
--  LocalWords:  maxWidth lem ctx concretize Poset acc usepkg toprule
--  LocalWords:  antisymmetric monotonicity inequation pareto concat
--  LocalWords:  O'Sullivan's benchmarking disjunctions snd fst Atze
--  LocalWords:  sublayout fitRibbon ribbonLength acknowledgements sc
--  LocalWords:  Facundo Domingez der Ploeg Arnaud Spiwack QuickSpec
--  LocalWords:  Smallbone onecolumn booktabs mkcols midrule mkrows
--  LocalWords:  SortedLabel typicalPerfTable JSON lrrr fname xform
--  LocalWords:  typicalPerfData outerBox scatterPlot xaxisLab xpart
--  LocalWords:  yaxisLab ypart hdist topOf performancePlotFull url
--  LocalWords:  performancePlotRandom natbib foldDoc propLeftUnit
--  LocalWords:  vspace propRightUnit propAssoc propTextAppend Xeon
--  LocalWords:  propTextEmpty propFlush propDisjAssoc propDistrL tbl
--  LocalWords:  combinatorially propDistrR propDistrFlush mathnormal
--  LocalWords:  propDisjCommut Dyck dyck randomDyck maxLen randomIO
--  LocalWords:  perf ICFP runtimes lexicographically mergeAll
