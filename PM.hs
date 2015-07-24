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
pretty print S-Expressions, represented as follows:


@haskell«
data SExpr where
   SExpr :: [SExpr] -> SExpr
   Atom :: String -> SExpr
  deriving Show
»


»
