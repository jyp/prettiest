{-# LANGUAGE GADTs, RecordWildCards #-}
import Data.Monoid
import Data.Function
import Data.List
import Data.Either

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

-- Invariants:
-- in a <|> b, contents a == contents b

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

type Measure = Int -> Int -> (Int,Int)

docMeasure :: Doc -> Measure
docMeasure d i c = minimum [(height s, c) | (s,c) <- eval d i c]

maxWidth = maximum . map length . lines
fits w s = maxWidth s <= w
height = length . lines
render w d = minimumBy (compare `on` height) $ filter (fits w) $ map fst $ eval d 0 0

x <+> y = x <> Spacing " " <> y
x </> y = x <> Line <> y

sep [] = mempty
sep xs = foldr1 (<+>) xs :<|> Align (foldr1 (</>) xs)
pretty (Atom s) = Text s
pretty (SExpr xs) = Text "(" <> (sep $ map pretty xs) <> Text ")"

abcd = SExpr $ map (Atom . (:[])) "abcd"
abcd4 = SExpr [abcd,abcd,abcd,abcd]
testData = SExpr [Atom "axbxcxd", abcd4] 

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


--------------
-- Derivation of the efficient renderer. (sketch)

-- 1. Reify the evaluation process

-- 1.a CPS
-- 1.b Defunctionalise

-- 2. Schedule the processing line by line

-- 3. Prune the dominated processes
-- Why can we do this?

-- We can verify that docMeasure is monotonous in the indentation level:

-- ∀d:Doc, ∀c:Column, ∀(i,j : Level), if i > j then docMeasure d i c > docMeasure d j c
-- Proof.
-- By ind. on doc.
-- Disj case: The min. of monotonous functions is monotonous.


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


test n = renderFast n $ pretty testData
main = do
  putStrLn $ test 15
  putStrLn $ test 21


