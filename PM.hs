{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Bib
import MarXup.Latex.Math (newtheorem, deflike)
import MarXup.Tex
import MarXup.LineUp.Haskell

main = renderTex "Prettiest.tex" (preamble (header >> mainText))

classUsed = SIGPlan

preamble body = do
  documentClass "article" []
  usepackage "inputenc" ["utf8x"]
  usepackage "tikz" []
  usepackage "graphicx" []
  usepackage "polytable" []
  newtheorem "principle" "Principle"
  env "document" body

principle :: TeX -> Tex SortedLabel
principle = deflike "Principle" "principle" mempty


header :: Tex ()
header = do
  maketitle
  -- abstract
  keywords classUsed $ [ "Pearl", "Pretty Printing"]


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
often represent programs, but not always.) I propose that two
principles are essential to pretty printing:

@pcp_layout<-principle« A pretty printer should make clever use of layout, to make it easy
             for a human to recognise the hierarchical organisation of data.»

@pcp_space<-principle«A pretty printer should minimize the amount of
space used to display the data.»


Even though they are not stated as such, these principle guide classic
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



»
