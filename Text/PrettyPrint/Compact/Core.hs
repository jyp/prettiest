{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Text.PrettyPrint.Compact.Core(Layout(..),Render(..),Document(..),Doc,annotate) where

import Data.List (sortOn,groupBy,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>))
import Data.String
import Data.Foldable (toList)

-- | Annotated string. We keep annotated segments in a separate list.
--
-- First argument is length.
--
data AS a = AS !Int [(Maybe a, String)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

asLength :: AS a -> Int
asLength (AS l _) = l

-- | Make non-annotated 'AS'
mkAS :: String -> AS a
mkAS s = AS (length s) [(Nothing, s)]

instance Semigroup (AS a) where
  AS i xs <> AS j ys = AS (i + j) (xs <> ys)

newtype L a = L (Seq (AS a)) -- non-empty sequence
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Semigroup (L a) where
  L (viewr -> xs :> x) <> L (viewl -> y :< ys) = L (xs <> singleton (x <> y) <> fmap (indent <>) ys)
      where n = asLength x
            indent = mkAS (Prelude.replicate n ' ')
  L _ <> L _ = error "<> @L: invariant violated, Seq is empty" 

instance Monoid (L a) where
   mempty = L (singleton (mkAS ""))
   mappend = (<>)

instance Layout (L a) where
   text = L . singleton . mkAS
   flush (L xs) = L (xs |> mkAS "")

instance Render L where
  annotatedRender f (L xs) = intercalate (toList xs)
    where
      f' (AS _ s) = foldMap (uncurry f) s
      sep = f Nothing "\n"

      intercalate []     = mempty
      intercalate (y:ys) = f' y `mappend` foldMap (mappend sep . f') ys

class Render d where
  annotatedRender :: Monoid r
                  => (Maybe a -> String -> r) -- ^ how to annotate the string. /Note:/ the annotation should preserve the visible length of the string.
                  -> d a                      -- ^ renderable
                  -> r

class Monoid d => Layout d where
  text :: String -> d
  flush :: d -> d
  -- render :: d -> String

class Layout d => Document d where
  (<|>) :: d -> d -> d

-- | type parameter is phantom.
data M a = M {height    :: Int,
              lastWidth :: Int,
              maxWidth  :: Int
              }
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Semigroup (M a) where
  a <> b =
    M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
       height = height a + height b,
       lastWidth = lastWidth a + lastWidth b}

instance Monoid (M a) where
  mempty = text ""
  mappend = (<>)

instance Layout (M a) where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}

class Poset a where
  (≺) :: a -> a -> Bool


instance Poset (M a) where
  M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2

mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy le = go
  where
    go [] xs = xs
    go xs [] = xs
    go (x:xs) (y:ys)
      | x `le` y  = x:go xs (y:ys)
      | otherwise = y:go (x:xs) ys

mergeAllBy :: (a -> a -> Bool) -> [[a]] -> [a]
mergeAllBy _ [] = []
mergeAllBy le (x:xs) = mergeBy le x (mergeAllBy le xs)

bests :: forall a. (Poset a)
      => (a -> a -> Bool)  -- total order: '<='
      -> [[a]] -> [a]
bests le = pareto' [] . mergeAllBy le

pareto' :: Poset a => [a] -> [a] -> [a]
pareto' acc [] = Prelude.reverse acc
pareto' acc (x:xs) = if any (≺ x) acc
                       then pareto' acc xs
                       else pareto' (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.

leFst :: Ord a => (a, b) -> (a, b) -> Bool
leFst = (<=) `on` fst

newtype Doc a = MkDoc [(M a,L a)] -- list sorted by lexicographic order for the first component
  deriving Show

instance Semigroup (Doc a) where
  MkDoc xs <> MkDoc ys = MkDoc $ bests leFst [ quasifilter (fits . fst) [x <> y | y <- ys] | x <- xs]
    where quasifilter p zs = let fzs = filter p zs
                             in if null fzs
                                then [minimumBy (compare `on` (maxWidth . fst)) zs]
                                else fzs

instance Monoid (Doc a) where
  mempty = text ""
  mappend = (<>)

-- TODO: make columns configurable
fits :: M a -> Bool
fits x = maxWidth x <= 80

instance Layout (Doc a) where
  flush (MkDoc xs) = MkDoc $ bests leFst $ map (sortOn fst) $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = pareto' [] $ sort $ (map flush xs)
  text s = MkDoc [text s]

instance Render Doc where
  annotatedRender _ (MkDoc []) = error "No suitable layout found."
  annotatedRender f (MkDoc xs@(x:_)) | maxWidth (fst x) <= 80 = annotatedRender f (snd x)
                            | otherwise = annotatedRender f $ snd (minimumBy (compare `on` (maxWidth . fst)) xs)

instance Document (Doc a) where
  MkDoc m1 <|> MkDoc m2 = MkDoc (bests leFst [m1,m2])


instance (Layout a, Layout b) => Layout (a,b) where
  text s = (text s, text s)
  flush (a,b) = (flush a, flush b)

instance (Document a, Document b) => Document (a,b) where
  (a,b) <|> (c,d) = (a<|>c,b<|>d)

instance (Poset a) => Poset (a,b) where
  (a,_) ≺ (b,_) = a ≺ b

instance IsString (Doc a) where
  fromString = text

-- | `<>` new annotation to the 'Doc'.
--
-- Example: 'Any True' annotation will transform the rendered 'Doc' into uppercase:
--
-- >>> let r = putStrLn . annotatedRender (\a x -> if a == Just (Any True) then map toUpper x else x)
-- >>> r $ text "hello" <$$> annotate (Any True) (text "world")
-- hello
-- WORLD
--
annotate :: forall a. Semigroup a => a -> Doc a -> Doc a
annotate a (MkDoc xs) = MkDoc (fmap (fmap annotateL) xs)
  where
    annotateL :: L a -> L a
    annotateL (L s) = L (fmap annotateAS s)

    annotateAS :: AS a -> AS a
    annotateAS (AS i s) = AS i (fmap annotatePart s)

    annotatePart (Nothing, s) = (ma, s)
    annotatePart (Just b,  s) = (Just (b <> a), s)

    ma = Just a

-- $setup
-- >>> import Text.PrettyPrint.Compact
-- >>> import Data.Monoid
-- >>> import Data.Char
