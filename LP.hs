
type Gen = LPT Supply

type Expr = LinExpr Var Double

type Doc = Expr -> Expr -> Gen (Int,Int)


space = text " "

empty = text ""

a <+> b = a <> space <> b

a </> b = a <> softBreak <> b

sep xs i c = do
  v <- choice
  (foldr (\x y -> x <> newLineIf v <> y) empty xs) i c

fsep = foldr (</>) empty

(<>) :: Doc -> Doc -> Doc
(a <> b) i c0 = do
  (h1,c1) <- a i c0
  (h2,c2) <- a i c1
  return (h1 + h2, c1)

text :: String -> Doc
text s i c = do
  return (0, c + length s)

newline :: Doc
newline i c = do
  c .<=. 80
  return (1,i)

newLineIf :: Var -> Doc
newLineIf cr i c = do
  return (cr,max i (c - 80 * cr))

choice = do
  v <- supplyNew
  setVarKind v BinVar
  return v

softBreak :: Doc
softBreak i c = do
  v <- choice
  newLineIf v i c

render x = do
  run $ do (h,c) <- x 0 0
           c .<= 80
           minimize h

main :: IO ()
main = print "yo"
