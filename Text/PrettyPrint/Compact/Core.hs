{-# LANGUAGE RecordWildCards #-}
module Text.PrettyPrint.Compact.Core(Doc(..),render,group,flatten,space,spacing,text) where

import Data.Monoid
import Data.Function (on)
import Data.List (partition,minimumBy,sort)
import Data.Either (partitionEithers)
import Data.String

type Indentation = Int

data Box = Str String | Spacing Indentation | NewLine {-^ not allowed in the input -}

instance IsString Doc where
  fromString = text

data Doc = Empty
         | Text Box
         | Line !Bool            -- True <=> when undone by group, do not insert a space
         | Cat Doc Doc
         | Nest Indentation Doc
         | Union Doc Doc           -- invariant: first lines of first doc longer than the first lines of the second doc
         | Column  (Indentation -> Doc)
         | Nesting (Indentation -> Doc)


instance Monoid Doc where
  mempty = Empty
  mappend = Cat

text :: String -> Doc
text = Text . Str

spacing = Text . Spacing

space = spacing 1

group :: Doc -> Doc
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line noSpace)  = if noSpace then Empty else space
flatten (Union x _y)    = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text

len :: Box -> Int
len (Str s) = length s
len (Spacing x) = x
len NewLine = 0

data Docs   = Nil
            | Cons !Indentation Doc Docs

data Process = Process {overflow :: Indentation
                       ,curIndent :: !Indentation -- current indentation
                       ,numToks :: Int -- total number of non-space tokens produced
                       ,tokens :: [Box] -- tokens produced, in reverse order
                       ,rest :: !Docs -- rest of the input document to process
                       }
-- The ⟨filtering⟩ step takes all process states starting at the current
-- line, and discards those that are dominated.  A process state
-- dominates another one if it was able to produce more tokens while
-- having less indentation. This is implemented by first sorting the
-- process states by following 'measure', and then applying the
-- 'filtering' function.

measure :: Process -> (Indentation, Int)
measure Process{..} = (curIndent, negate numToks)

instance Eq Process where
  (==) = (==) `on` measure
instance Ord Process where
  compare = compare `on` measure

filtering :: [Process] -> [Process]
filtering (x:y:xs) | numToks x >= numToks y = filtering (x:xs)
                   | otherwise = x:filtering (y:xs)
filtering xs = xs

renderAll :: Double -> Indentation -> Doc -> [Box]
renderAll rfrac w doc = reverse $ loop [Process 0 0 0 [] $ Cons 0 doc Nil]
    where
      loop ps = case dones of
        ((_,done):_) -> done -- here we could attempt to do a better choice. Seems to be fine for now.
        [] -> case conts of
          (_:_) -> loop $ filtering $ sort $ conts -- See the comment ⟨filtering⟩ above
          [] -> case conts'over of
            (_:_) -> loop [minimumBy (compare `on` overflow) conts'over]
            [] -> case dones'over of
              ((_,done):_) -> done
              [] -> [Str "Pretty print: Panic"]

        where
          -- advance all processes by one line (if possible)
          ps' = concatMap (\Process{..} -> rall numToks tokens curIndent curIndent rest) ps
          -- Have some processes reached the end? With some overflow?
          (dones0,conts0) = partitionEithers ps'
          (conts,conts'over) = partition (\p -> overflow p <= 0) conts0
          (dones,dones'over) = partition (\(o,_) -> o <= 0) dones0

      -- r :: the ribbon width in characters
      r  =  max 0 (min w (round (fromIntegral w * rfrac)))

      -- Automatically inserted spacing does not count as doing more production.
      count (Spacing _) = 0
      count (Str _) = 1

      -- Compute the state(s) after reaching the end of line.
      -- rall :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      rall :: Int -> [Box] -> Indentation -> Indentation -> Docs -> [Either (Indentation,[Box]) Process]
      rall nts ts n k ds0 = case ds0 of
         Nil ->  [Left $ (overflow,ts)] -- Done!
         Cons i d ds -> case d of
            Empty       -> rall nts ts n k ds
            Text s      -> let k' = k+len s in seq k' (rall (nts+count s) (s:ts) n k' ds)
            Line _      -> [Right $ Process overflow i nts (Spacing i:NewLine:ts) ds] -- "yield" when the end of line is reached
            Cat x y     -> rall nts ts n k (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in seq i' (rall nts ts n k (Cons i' x ds))
            Union x y   -> rall nts ts n k (Cons i x ds) ++ rall nts ts n k (Cons i y ds)
            Column f    -> rall nts ts n k (Cons i (f k) ds)
            Nesting f   -> rall nts ts n k (Cons i (f i) ds)
        where overflow = negate $ min (w - k) (r - k + n)


layout :: [Box] -> String
layout (Str s:xs) = s ++ layout xs
layout (NewLine:xs) = '\n' : layout xs
layout (Spacing x:xs) = replicate x ' ' ++ layout xs

render :: Double -> Indentation -> Doc -> String
render rfrac w d = do
  layout $ renderAll rfrac w d

instance Show Doc where
  show = render 0.8 80
