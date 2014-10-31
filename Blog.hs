{-# LANGUAGE GADTs #-}
import Data.Monoid
import Data.Function
import Data.List

data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
 deriving Show

data Doc where
  Line :: Doc
  Nil :: Doc
  (:<>) :: Doc -> Doc -> Doc
  Text :: String -> Doc
  Nest :: Int -> Doc -> Doc
  Align :: Doc -> Doc
  (:<|>) :: Doc -> Doc -> Doc -- ^ Attn: INVARIANT
  Spacing :: String -> Doc 

instance Monoid Doc where
  mempty = Nil
  mappend = (:<>)

contents :: Doc -> [String]
contents (Spacing _) = []
contents Nil = []
contents Line = []
contents (d1 :<> d2) = contents d1 <> contents d2
contents (Text x) = [x]
contents (Align x) = contents x
contents (x :<|> y) = contents x

type Eval = Int -> Int -> [(String,Int)]

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

maxWidth = maximum . map length . lines
fits w s = maxWidth s <= w
height = length . lines
render w d = minimumBy (compare `on` height) $ filter (fits w) $ map fst $ eval d 0 0

x <+> y = x <> Spacing " " <> y
x </> y = x <> Line <> y

sep [] = mempty
sep xs = foldr1 (<+>) xs :<|> foldr1 (</>) xs
pretty (Atom s) = Text s
pretty (SExpr xs) = Text "(" <> Align (sep $ map pretty xs) <> Text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 
  
test n = layout n $ pretty testData
main = do
  putStrLn $ test 15
  putStrLn $ test 21
