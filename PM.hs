{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (newtheorem, deflike)
import MarXup.Tex
import MarXup.Diagram
import MarXup.LineUp.Haskell
import MarXup.Verbatim
import MarXup.Latex.Math (ensureMath)

main = renderTex "Prettiest" (preamble (header >> mainText))

classUsed = SIGPlan

preamble body = do
  documentClass "../PaperTools/latex/sigplanconf" []
  usepackage "inputenc" ["utf8x"]
  usepackage "tikz" []
  usepackage "graphicx" []
  usepackage "polytable" []
  newtheorem "principle" "Principle"
  title "An Insanely Pretty Printer"
  authorinfo Plain [AuthorInfo "Jean-Philippe Bernardy" "bernardy@chalmers.se" "CTH"]
  env "document" body

principle :: TeX -> TeX -> Tex SortedLabel
principle titl = deflike "Principle" "principle" titl


header :: Tex ()
header = do
  maketitle
  -- abstract
  -- keywords classUsed $ [ "Pearl", "Pretty Printing"]


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


A pretty printing algorithm renders data structures in a way which
makes them pleasant to read. (The data structures in question are
often represent programs, but not always.) I propose the following
three laws of pretty printing:

@pcp_visibility<-principle«Visibility»«Pretty printer shall
layout all its output within the width of the page.»

@pcp_compact<-principle«compactness»«A pretty printer shall minimize the amount of
space used to display the data.»

@pcp_layout<-principle«layout»« A pretty printer shall make clever use of layout, to make it easy
             for a human to recognise the hierarchical organisation of data.»

Furthermore, the first law takes precedence over the second one, which itself takes precedence over the third.


Even though they are not stated as such, these laws guide classic
functional pretty printing libraries, such as Hughes' and Wadler's.
Indeed, the functional programming community often uses pretty
printing to showcase proper program design: Hughes' pretty printer
remains an influential example of functional programming design, while
Wadler's has appeared as chapter of of a book dedicated to "beautiful
code".

In addition of esthetical and pedagogical value, Hughes and Wadler's
provide practical implementations which form the basis of pretty
printing packages which remain popular today:

* [pretty](https://hackage.haskell.org/package/pretty)
* [wl-pprint](https://hackage.haskell.org/package/wl-pprint)

In this paper, I propose a new design for a pretty printing
library. The interface is inspired by Hughes and Wadler's, but is
subtly different. In contrast to Hughes and Wadler, my primary goal is
to abide by the principles of pretty printing as defined above;
efficiency is a secondary concern. (Yet the final result is reasonably
efficient.)  As Hughes and Wadler, I will aim at using a
mathematically-oriented methodology and a clean design. Additionally,
will draw general conclusions on how to improve on functional
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

> data SExpr where
>   SExpr :: [SExpr] -> SExpr
>   Atom :: String -> SExpr
>  deriving Show

Using the above representation, the S-Expr @teletype«(a b c d)» has the
following encoding:

> abcd :: SExpr
> abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]

Let us specify pretty printing of S-Expr as follows. In a pretty
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

The goal of the pretty printer is to print a given s-expression in as
few lines as possible, while respecting the above rules, and fitting
within the width of a page.

Traditionally, a function pretty printing library gives us the means to express the
specification of possible pretty layouts, and automatically pick the
prettiest. 
We will not depart from the tradition. As Hughes', our library will allow to express both vertical (`$$`) and
horizontal (`<>`) composition of documents, as well as embedding raw
text (`text`) and provide automatic choice between layouts (`<|>`). At this
stage, we keep the representation of documents abstract using a
typeclass, provide the above combinators, as well as means of @hask«render»ing a document:

@haskell«
text   :: Doc d => String -> d
(<>)   :: Doc d => d -> d -> d
($$)   :: Doc d => d -> d -> d
(<|>)  :: Doc d => d -> d -> d
render :: Doc d => d -> String
»

We can then define a few useful combinators on top of the above: the
@haskell«empty» document; concatenation with an intermediate space @haskell«(<+>)»; vertical and
horizontal concatenation of multiple documents.

@haskell«
empty :: Layout d => d
empty = text ""

(<+>) :: Layout d => d -> d -> d
x <+> y = x <> text " " <> y

hsep,vcat :: Doc d => [d] -> d
vcat = foldr1 ($$)
hsep = foldr1 (<+>)
»

We can furthermore define automatic choice between horizontal and vertical
concatenation:

@haskell«
sep :: Doc d => [d] -> d
sep [] = empty
sep xs = hsep xs <|> vcat xs
»

Turning S-Expressions into a pretty document is then child's play:

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
to pretty print the following s-expr (which is specially crafted to
demonstrate issues with both Hughes and Wadler libraries):

> testData :: SExpr
> testData = SExpr [SExpr [Atom "12345", abcd4],
>                   SExpr [Atom "12345678", abcd4]]
>   where abcd4 = SExpr [abcd,abcd,abcd,abcd]

We would like elements inside an S-Expr to be either
aligned vertically (for legibility, @pcp_layout),
or concatenated horizontally (for compactness, @pcp_compact).
The second option will be preferred over the first, as long
as the text fits within the page width.

Thus, printed on a 80-column-wide page, we'd like to get:

@verbatim«
12345678901234567890123456789012345678901234567890123456789012345678901234567890

((12345 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
 (12345678 ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d))))
»

(The first line is a helper showing the column of a given character.)
Printed on a 20-column-wide page, we'd like to get:

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

Unfortunately, in this case, and using Hughes' library, we would get the following output:

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
the capability to express that a multi-line sub-document $b$ should be
laid out to the right of a document $a$: $a$ must be put below $b$.
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

It's not too bad! But it inserts a spurious line break after the atom
@teletype«12345678». While this may be acceptable to some, I find it
disappointing for two reasons. First, spurious line breaks may appear
in many situations. Second, the element which is rejected to a next
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

@horizCat



»

lineHeight = 10

abstrLayout :: Expr -> Diagram (Object,Point)
abstrLayout lastWidth = do
  bx <- box
  let points@[_nw,_ne,_se,after,_sse,_sw]
        = map (p0+)
          [bx NW
          ,bx NE
          ,bx SE + Point 0 lineHeight
          ,bx SW + Point lastWidth lineHeight
          ,bx SW + Point lastWidth 0
          ,bx SW]
      p = polygon point
  polygon p
  return (Object p bx, after)

twoLayouts :: Diagram (Object,Point,Object)
twoLayouts = do
  (a,aa) <- abstrLayout 30
  (b,_) <- abstrLayout 30
  width a === 200
  width b === 150
  height a === 6 *- lineHeight
  height b === 6 *- lineHeight
  return (a,aa,b)


horizCat :: Dia
horizCat = do
  (a,mid,b) <- twoLayouts
  op <- labelObj "<>"
  let lhsObjs = [a,op,b] 
  spread hdist lhsObjs
  align ypart $ map (#Center) $ lhsObjs
  lhs <- boundingBox lhsObjs
  
  (a',mid',b') <- twoLayouts
  b'#NW .=. mid'

  hdist lhs a' === 20
  return () 

spec = haskell

verbatim :: Verbatim () -> TeX
verbatim (Verbatim s _) =
    env "verbatim" (tex s)

hask = ensureMath . cmd "mathsf"

