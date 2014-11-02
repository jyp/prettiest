% The Prettiest Printer
% a note by Jean-Philippe Bernardy

Parsing and pretty printing are perhaps the two most classic problems
in the functional programming repertoire. I having turned my attention
to parsing (many times) before. Before I retire, I feel that I should
to try my hands on pretty printing.

The state of the art.
=====================

Even today, pretty printing in Haskell is mostly backed by two classic
libraries, either:

1.  The Hughes-Peyton Jones library. The design is [described by
    Hughes](http://belle.sourceforge.net/doc/hughes95design.pdf) in
    *The Design of a Pretty-printing Library*. It has then been
    adopted (and modified) by Peyton Jones, and was distributed with GHC
    for a long time, making it the *de-facto* standard pretty printer.
    It is now available on Hackage in the eponymous package
    [pretty](https://hackage.haskell.org/package/pretty). I believe that
    this remains the dominant design, perhaps disputed by...

2.  The Wadler-Leijen library. In the penultimate chapter of *The Fun
    of Programming*, Wadler re-constructs a pretty printing library
    from scratch. Keeping true to Hughes in particular and the general
    functional programming tradition in general, Wadler starts by
    specifying his library using equational laws, and derives an
    implementation. Leijen took Wadler's implementation and modified it
    to increase its expressivity (but more on that later). The result is
    available in the
    [wl-pprint](https://hackage.haskell.org/package/wl-pprint)
    package.

Not. Pretty. Enough.
--------------------

As it happens, I am dissatisfied with the outputs produced by either
libraries. At the risk of appearing ungrateful to the masters, I'll
spend some effort to back this claim.

### Hughes

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

(I'll refrain the urge to be more specific at this point).

For the sake of the argument, let's pretend we want to pretty print the
following s-expr:

``` {.example}
abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
```

Printed on a wide page, we'd get:

``` {.example}
(axbxcxd ((a b c d) (a b c d) (a b c d) (a b c d) (a b c d)))
```

In a narrow page (whose width is indicated by the row of hashes), what
we'd get from Hughes' library is the following output:

``` {.example}
###############
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
###############
```

This does not quite cut it. Our senses of aesthetics are tingling...
this is not pretty enough. Why can't we get the following?

``` {.example}
###############
(axbxcxd
 ((a b c d)
  (a b c d)
  (a b c d)
  (a b c d)
  (a b c d))
```

The thing is, Hughes believes that "it would be unreasonably inefficient
for a pretty-printer do decide whether or not to split the first line of
a document on the basis of the content of the last." (sec. 7.4 of his
paper). Therefore, he chooses a greedy algorithm, which tries to fit as
much as possible on a single line, without regard for what comes next.
In our example, the algorithm fits `(axbxcxd ((a`, but then it has
committed to a very deep indentation level, which forces a
less-than-pretty outcome for the remainder of the document.

### Wadler

Wadler's design fares somewhat better. It does not suffer from the above
problem... *by default*. That is, it lacks the capability to express
that sub-documents should be vertically aligned --- compositionally.

Let us illustrate. Using Wadler's library, one might specify pretty
printing of s-exprs as follows (see Wadler's paper is there is any doubt
on the meaning of the following):

``` {.example}
x </> y = x <> line <> y
sep = foldr empty (</>)
```

``` {.example}
pretty (SExpr xs) = group $ "(" <> nest 1 (sep $ map pretty xs) <> ")"
pretty (Atom x) = text x
```

which appears to do the trick; we get:

``` {.example}
###############
(axbxcxd
 ((a b c d)
  (a b c d)
  (a b c d)
  (a b c d)
  (a b c d))
```

However, the `group` combinator does not quite behave as I'd like.
What `group` does is to allow its argument to be laid out on a single
line, instead of multiple ones. Hence, we can put two elements next to
each other *only if* they are flattened. This means that if we typeset
the same s-expr, but in a slightly wider page, we get the same output:

``` {.example}
#####################
(axbxcxd
 ((a b c d)
  (a b c d)
  (a b c d)
  (a b c d)
  (a b c d))
```

whereas my iThings-saturated eyes crave something more pleasing:

``` {.example}
#####################
(axbxcxd ((a b c d)
          (a b c d)
          (a b c d)
          (a b c d)
          (a b c d))
```

At this point, the reader may raise two objections:

-   Objection 1: *All this fuss for ONE LINE?*
-   Reply 1: Every computer-science academic has once in their lifetime
    been in a killing mood due to a one-line-to-long printout causing a
    paper to spill over the page limit. So that extra line saved *is*
    worth something. Plus, we can construct examples where more space
    can be saved.

-   Objection 2: *Leijen's extension of Wadler's design solves the
    issue: it provides an `align` combinator.*
-   Reply 2: Yes, but: it exhibits the same symptoms as Hughes' library.

    Aside: In his paper, Wadler proves that his library produces the
    shortest output. But, Leijen's extension breaks this invariant. This
    makes me suspect that the extension was done on the implementation
    directly rather than on the design. (If there were a functional
    programming inquisition, they should knock on Leijen's door.)

In search of the prettiest output
=================================

API
---

Before discussing possible algorithms, we need to chose wisely the the
document-description language that we accept. I'll daringly stand on
Phil's strong shoulders (himself balancing on John; what a feat), and
propose the following set of combinators:

-   `empty`: The empty document
-   `(\<\>)`: concatenation
-   `line`: insert a new line (unconditionally)
-   `text`: insert a meaningful piece of text
-   `nest`: nest the argument
-   `align`: align the documents in the argument
-   `(\<|\>)`: disjunction of layouts
-   `spacing`: non-meaningful text (spaces or typographical marks)

which can be implemented as follows:

``` {.example}
data Doc where
  Line :: Doc
  Nil :: Doc
  (:<>) :: Doc -> Doc -> Doc
  Text :: String -> Doc
  Nest :: Int -> Doc -> Doc
  Align :: Doc -> Doc
  (:<|>) :: Doc -> Doc -> Doc -- ^ Attn: INVARIANT
  Spacing :: String -> Doc
```

The main pitfall of the above API is that disjunction (\<|\>) can only
take arguments which differ in layout. That is, it requires the two
documents with the same `content`, defined as follows:

``` {.example}
contents :: Doc -> [String]
contents (Spacing _) = []
contents Nil = []
contents Line = []
contents (d1 :<> d2) = contents d1 <> contents d2
contents (Text x) = [x]
contents (Align x) = contents x
contents (x :<|> y) = contents x
```

(Note that the above function recursively relies on the invariant being
verified.)

Other pitfalls include that text and spacing may not contain any
newline, and nesting may not be negative.

### Example

Using the above combinators, we can pretty print s-exprs as follows:

``` {.example}
x <+> y = x <> Spacing " " <> y
x </> y = x <> Line <> y
```

``` {.example}
sep [] = mempty
sep xs = foldr1 (<+>) xs :<|> foldr1 (</>) xs
pretty (Atom s) = Text s
pretty (SExpr xs) = Text "(" <> Align (sep $ map pretty xs) <> Text ")"
```

Semantics
---------

Now that we have our API, we can specify how to render documents. I
could start by stating a few laws on the API (in particular all laws
stated by Wadler should hold) but I'll specify it directly using a
compositional interpretation. The interpretation of a document is a
non-deterministic function from the current indentation level and
current column to a text and a final column.

Using lists for non-determinism, we have:

``` {.example}
type Eval = Int -> Int -> [(String,Int)]
```

The evaluation function is then the following.

``` {.example}
eval :: Doc -> Eval
eval (Text s) i c = return (s, c + length s)
eval (Spacing s) i c = return (s, c + length s)
eval Nil i c = return ("",c)
eval (Align d) i c = eval d c c
eval (Nest j d) i c = eval d (i+j) c
eval Line i c = return ('\n' : replicate i ' ', i)
eval (d1 :<> d2) i c = do
  (t1,c1) <- eval d1 i c
  (t2,c2) <- eval d2 i c1
  return (t1 ++ t2, c2)
eval (d1 :<|> d2) i c = eval d1 i c ++ eval d2 i c
```

Given the use of monadic syntax to handle list-non-determinism, the
interpretation of `text`, `spacing`, `empty`, `<>`, and even `<|>`
reserve no particular surprise. The interesting bit is the interplay
between `line`, `nest` and `align`.

The indentation level is implemented by inserting a certain number of
spaces after moving to the next `Line`. `Nest`-ing is defined by
increasing the indentation level. `Align`-ing means setting the
indentation level to the current column.

Finally, we can define the prettiest rendering as that which

-   fits the page and
-   uses the smallest amount of lines

(This is not quite the full truth: sometimes no layout fits the page,
and we want to pick that with the least overflow. We'll leave such
details to the implementer.)

Fitting the page means that the line width is less than the page width:

``` {.example}
maxWidth = maximum . map length . lines
fits w s = maxWidth s <= w
```

The final renderer is thus:

``` {.example}
height = length . lines
render w d = minimumBy (compare `on` height) $ filter (fits w) $ map fst $ eval d 0 0
```

The above renderer satisfies our needs: it finds the prettiest layout.
Yet, we should not expect to get results quickly. A document may contain
hundreds of disjunctions, and if we exhaustively search a space that
big, even the legendary long-lasting batteries of our iPads(tm) will die
before anything can be printed.

Implementation
--------------

Fortunately, there is a way out of this tar-pit. The trick is to explore
the search space *line by line*. That is, every time we find the
`Line` combinator, we stash the current partial result for later
examination. Eventually, all pending states will be stashed. We can then
*prune out* useless, dominated states, and resume the search. There
remains to define when a state is dominated.

For each state *t*, we define:

-   *i(t)*: the indentation of the next line (remember that we stopped
    at a given line)
-   *p(t)*: the progress inside the document, defined as the number of
    tokens printed so far. Remember that disjuncted documents must have
    the same contents, so it is meaningful to compare *p(t)* and *p(u)*
    for every pair of processes *(t,u)*.

Definition: *t* dominates *u* iff. *i(t) \< i(u)* and *p(t) \>= p(u)*.

Indeed, if *u* is at a higher indentation level, it has much less space
to print the rest of the document (remember that indentation is always
positive). Therefore, if it is also late in the production of tokens,
there is no hope for *u* to catch up with *t*. (The proof of this fact
may come to an academic journal in the future. And it certainly does not
fit in the margin.)

Consequently, if there is a finite number *l* of indentation levels
(traditionally *l=80*), then we have only to consider *l* solutions
after each line break. There is no exponential blow up.

The code implementing the above idea fits just about on a page:

``` {.example}
type Docs = [(Int,Doc)]
data Process = Process {curIndent :: Int -- current indentation
                       ,progress :: Int
                       ,tokens :: [String] -- tokens produced, in reverse order
                       ,rest :: Docs  -- rest of the input document to process
                       }
measure :: Process -> (Int, Int)
measure Process{..} = (curIndent, negate progress)

filtering :: [Process] -> [Process]
filtering (x:y:xs) | progress x >= progress y = filtering (x:xs)
                   | otherwise = x:filtering (y:xs)
filtering xs = xs

renderFast :: Int -> Doc -> String
renderFast w doc = concat $ reverse $ loop [Process 0 0 [] $ [(0,doc)]]
    where
      loop ps = case dones of
        (done:_) -> done
        [] -> case conts of
          (_:_) -> loop $ filtering $ sortBy (compare `on` measure) $ conts
          [] -> ["Panic: overflow"]
        where
          ps' = concatMap (\Process{..} -> rall progress tokens curIndent rest) ps
          (dones,conts) = partitionEithers ps'

      rall :: Int -> [String] -> Int -> Docs -> [Either [String] Process]
      rall p ts k ds0 | k > w = []
      rall p ts k ds0 = case ds0 of
         [] ->  [Left ts] -- Done!
         (i,d):ds -> case d of
            Nil       -> rall p ts k ds
            Text s    -> rall (p+1) (s:ts) (k+length s) ds
            Spacing s -> rall (p  ) (s:ts) (k+length s) ds
            Line      -> [Right $ Process i p (('\n':replicate i ' '):ts) ds]
            x :<> y   -> rall p ts k ((i,x):(i,y):ds)
            Nest j x  -> rall p ts k ((i+j,x):ds)
            x :<|> y  -> rall p ts k ((i,x):ds) ++ rall p ts k ((i,y):ds)
            Align x   -> rall p ts k ((k,x):ds)
```

Coda
====

The above has been inspired by two implementations of pretty printers
that I've made. One is a regular pretty printing library, [available on
hackage](https://hackage.haskell.org/package/pretty-compact) which is
(nearly) a drop-in replacement for the `print-wl` package.

Another is part of the
[marxup](https://hackage.haskell.org/package/marxup) package, which is a
Haskell layer on top of the Latex document-preparation system.

Happy pretty printing!


