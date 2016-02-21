{-# LANGUAGE OverloadedStrings #-}

module Text.PrettyPrint.Compact (
   -- * Documents
   Doc,

   -- * Basic combinators
   module Data.Monoid, text, flush, char,

   hang, encloseSep, list, tupled, semiBraces,

   -- * Operators
   (<+>), ($$),

   -- * List combinators
   hsep, sep, hcat, vcat, cat, punctuate,

   -- * Bracketing combinators
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Character documents
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
   squote, dquote, semi, colon, comma, space, dot, backslash, equals,

   -- * Primitive type documents
   string, int, integer, float, double, rational,
   bool,

   -- * Rendering
   render,

   -- * Undocumented
   -- column, nesting, width
   ) where

import Data.Monoid

import Text.PrettyPrint.Compact.Core as Text.PrettyPrint.Compact

-- | The document @(list xs)@ comma separates the documents @xs@ and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
list :: [Doc] -> Doc
list            = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: [Doc] -> Doc
tupled          = encloseSep lparen   rparen  comma


-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: [Doc] -> Doc
semiBraces      = encloseSep lbrace   rbrace  semi

-- | The document @(enclosure l r sep xs)@ concatenates the documents
-- @xs@ separated by @sep@ and encloses the resulting document by @l@
-- and @r@. The documents are rendered horizontally if that fits the
-- page. Otherwise they are aligned vertically. All separators are put
-- in front of the elements. For example, the combinator 'list' can be
-- defined with @enclosure@:
--
-- > list xs = enclosure lbracket rbracket comma xs
-- > test    = text "list" <+> (list (map int [10,200,3000]))
--
-- Which is layed out with a page width of 20 as:
--
-- @
-- list [10,200,3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [10
--      ,200
--      ,3000]
-- @
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right sep ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> cat (zipWith (<>) (left : repeat sep) ds) <> right

-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
-- document @p@ except for the last document.
--
-- > someText = map text ["words","in","a","tuple"]
-- > test     = parens (align (cat (punctuate comma someText)))
--
-- This is layed out on a page width of 20 as:
--
-- @
-- (words,in,a,tuple)
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- (words,
--  in,
--  a,
--  tuple)
-- @
--
-- (If you want put the commas in front of their elements instead of
-- at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @(\<$\>)@.
--
sep :: [Doc] -> Doc
sep [] = mempty
sep xs = hsep xs <|> vcat xs


-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, than
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (\<\/\>) empty xs
fillSep :: [Doc] -> Doc
fillSep         = foldDoc (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with @(\<+\>)@.
hsep :: [Doc] -> Doc
hsep            = foldDoc (<+>)



-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @(\<$$\>)@.
--
-- > cat xs  = group (vcat xs)
cat :: [Doc] -> Doc
cat [] =  mempty
cat xs = hcat xs <> vcat xs

-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, than inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (\<\/\/\>) empty xs
fillCat :: [Doc] -> Doc
fillCat         = foldDoc (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: [Doc] -> Doc
hcat            = foldDoc (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @($$)@.
vcat :: [Doc] -> Doc
vcat            = foldDoc ($$)

foldDoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
foldDoc _ []       = mempty
foldDoc f ds       = foldr1 f ds

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with a
-- @space@ in between.  (infixr 6)
(<+>) :: Doc -> Doc -> Doc
x <+> y         = x <> space <> y

-- | The document @(x \<\/\> y)@ puts @x@ and @y@ either next to each other
-- (with a @space@ in between) or underneath each other. (infixr 5)
(</>) :: Doc -> Doc -> Doc
x </> y         = ((x <> space) <|> flush x) <> y

-- | The document @(x \<\/\/\> y)@ puts @x@ and @y@ either right next to each
-- other or underneath each other. (infixr 5)
(<//>) :: Doc -> Doc -> Doc
x <//> y        = (x <|> flush x) <> y


-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@ with
-- a linebreak in between. (infixr 5)
(<$$>) :: Doc -> Doc -> Doc
x <$$> y = flush x <> y


-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: Doc -> Doc
squotes         = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: Doc -> Doc
dquotes         = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: Doc -> Doc
braces          = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: Doc -> Doc
parens          = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: Doc -> Doc
angles          = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: Doc -> Doc
brackets        = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x   = l <> x <> r

char :: Char -> Doc
char x = text [x]

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc
lparen          = char '('
-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc
rparen          = char ')'
-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc
langle          = char '<'
-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc
rangle          = char '>'
-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc
lbrace          = char '{'
-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc
rbrace          = char '}'
-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc
lbracket        = char '['
-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc
rbracket        = char ']'


-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc
squote          = char '\''
-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc
dquote          = char '"'
-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc
semi            = char ';'
-- | The document @colon@ contains a colon, \":\".
colon :: Doc
colon           = char ':'
-- | The document @comma@ contains a comma, \",\".
comma :: Doc
comma           = char ','

-- | The document @dot@ contains a single dot, \".\".
dot :: Doc
dot             = char '.'
-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc
backslash       = char '\\'
-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc
equals          = char '='

-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.
string :: String -> Doc
string = vcat . map text . lines

bool :: Bool -> Doc
bool b          = text (show b)

-- | The document @(int i)@ shows the literal integer @i@ using
-- 'text'.
int :: Int -> Doc
int i           = text (show i)

-- | The document @(integer i)@ shows the literal integer @i@ using
-- 'text'.
integer :: Integer -> Doc
integer i       = text (show i)

-- | The document @(float f)@ shows the literal float @f@ using
-- 'text'.
float :: Float -> Doc
float f         = text (show f)

-- | The document @(double d)@ shows the literal double @d@ using
-- 'text'.
double :: Double -> Doc
double d        = text (show d)

-- | The document @(rational r)@ shows the literal rational @r@ using
-- 'text'.
rational :: Rational -> Doc
rational r      = text (show r)



-- | The hang combinator implements hanging indentation. The document
-- @(hang i x)@ renders document @x@ with a nesting level set to the
-- current column plus @i@. The following example uses hanging
-- indentation for some text:
--
-- > test  = hang 4 (fillSep (map text
-- >         (words "the hang combinator indents these words !")))
--
-- Which lays out on a page with a width of 20 characters as:
--
-- @
-- the hang combinator
--     indents these
--     words !
-- @
--
-- The @hang@ combinator is implemented as:
--
-- > hang i x  = align (nest i x)
hang :: Int -> Doc -> Doc -> Doc
hang n x y = (x <+> y) <|> (x $$ nest' n y)


-- | The document @(nest i x)@ renders document @x@ with the current
-- indentation level increased by i (See also 'hang', 'align' and
-- 'indent').
--
-- > nest 2 (text "hello" <$$$> text "world") <$$$> text "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @


space = text " "


nest' n x = spaces n <> x

spaces :: Int -> Doc
spaces n = text $ replicate n ' '

($$) :: Doc -> Doc -> Doc
a $$ b = flush a <> b
