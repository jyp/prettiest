{-# LANGUAGE FlexibleContexts, GADTs #-}
import Data.LinearProgram
import Data.LinearProgram.LinExpr
-- import Control.Monad.Supply
-- import Control.Monad.LPMonad
import qualified Data.Map as M

type Gen = LPT Var Double VSupply

type Expr = LinExpr Var Double

type Doc = Expr -> Expr -> Gen (Expr,Expr)

var :: Num c => v -> LinExpr v c
var x = LinExpr (M.singleton x 1) 0

(.<=.) :: Expr -> Expr -> Gen ()
a .<=. b = do
  f `geqTo` (negate c)
  where LinExpr f c = b ^-^ a
        -- a <= b
        -- b - a >= 0
        -- f + c >= 0
        -- f >= -c

space :: Doc
space = text " "

empty :: Doc
empty = text ""

(<+>) :: Doc -> Doc -> Doc
a <+> b = a <> space <> b

(</>) :: Doc -> Doc -> Doc
a </> b = a <> softBreak <> b

sep :: [Doc] -> Doc
sep xs i c = do
  v <- choice
  (foldr (\x y -> x <> newLineIf v <> y) empty xs) i c

-- fsep = foldr (</>) empty

(<>) :: Doc -> Doc -> Doc
(a <> b) i c0 = do
  (h1,c1) <- a i c0
  (h2,c2) <- b i c1
  return (h1 ^+^ h2, c2)

con :: Int -> Expr
con x = LinExpr zero (fromIntegral x)

text :: String -> Doc
text s _ c = do
  return (zero, c ^+^ con (length s))

pageWidth :: Int
pageWidth = 80

newline :: Doc
newline i c = do
  c .<=. con pageWidth
  return (con 1,i)

lpMax :: Expr -> Expr -> Gen Expr
lpMax a b = do
  v <- supplyNew
  setVarKind v ContVar
  setVarBounds v (LBound 0)
  a .<=. var v 
  b .<=. var v 
  return (var v)

newLineIf :: Var -> Doc
newLineIf cr i c = do
  c .<=. con pageWidth
  m <- lpMax i (c ^-^ fromIntegral pageWidth *^ var cr)
  return (var cr,m)

choice :: Gen Var
choice = do
  v <- supplyNew
  setVarKind v BinVar
  return v

softBreak :: Doc
softBreak i c = do
  v <- choice
  newLineIf v i c

render :: Doc -> IO ()
render x = do
  let problem = runVSupply $ execLPT $
        do (LinExpr h _,c) <- x zero zero
           c .<=. con pageWidth
           setObjective h
           setDirection Min
  print problem
  solution <- glpSolveVars mipDefaults  problem
   -- {brTech = HybridP}
  print solution

data SExpr where
  SExpr :: [SExpr] -> SExpr
  Atom :: String -> SExpr
 deriving Show

abcd :: SExpr
abcd = SExpr [Atom "a",Atom "b",Atom "c",Atom "d"]

testData :: SExpr
testData = SExpr [SExpr [Atom "12345", abcd4],
                  SExpr [Atom "12345678", abcd4]]
  where abcd4 = SExpr [abcd,abcd,abcd,abcd]

testData2 = SExpr (replicate 10 testData)
testData4 = SExpr (replicate 10 testData2)
testData8 = SExpr (replicate 10 testData4)

pretty :: SExpr -> Doc
pretty (Atom x) = text x
pretty (SExpr xs) = text "(" <> sep (map pretty xs) <> text ")"

main :: IO ()
main = do
  render $ pretty $ testData2
