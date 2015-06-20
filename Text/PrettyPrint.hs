-- | Insane drop-in replacement for John's pretty printer
module Text.PrettyPrint (module Text.PrettyPrint, Doc) where

import Data.String
import qualified Insane as I
import Insane (Doc)

empty :: Doc
empty = I.empty

infixl 6 <>
infixl 6 <+>
infixl 5 $$

(<>),(<+>),($$) :: Doc -> Doc -> Doc
(<+>) = (I.<+>)
($$)  = (I.$$)
(<>)  = (I.<->)

hsep,vcat,sep :: [Doc] -> Doc
hsep = I.hsep
vcat = I.vcat
sep  = I.sep

fsep :: [Doc] -> Doc
fsep xs = sep xs
  -- foldr (I.<|>) (hsep xs) [ hsep l $$ fsep r | (l,r) <- drop 1 (chops xs) ]

chops :: [a] -> [([a],[a])]
chops xs = [ splitAt n xs | n <- [0..length xs-1] ]

render :: Doc -> String
render = I.render

text :: String -> Doc
text = I.text

nest :: Int -> Doc -> Doc
nest = I.nest'

hang :: Doc -> Int -> Doc -> Doc
hang = flip I.hang

punctuate :: Doc -> [Doc] -> [Doc]
punctuate d []     = []
punctuate d [x]    = [x]
punctuate d (x:xs) = (x <> d) : punctuate d xs

int :: Int -> Doc
int = text . show

integer :: Integer -> Doc
integer = text . show

parens :: Doc -> Doc
parens p = char '(' <> p <> char ')'

brackets :: Doc -> Doc
brackets p = char '[' <> p <> char ']'

braces :: Doc -> Doc
braces p = char '{' <> p <> char '}'

char :: Char -> Doc
char x = text [x]

instance Show Doc where
  show = render

instance IsString Doc where
  fromString = text
