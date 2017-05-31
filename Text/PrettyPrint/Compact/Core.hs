{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Text.PrettyPrint.Compact.Core(Layout(..),Render(..),Document(..),Doc,annotate) where

import Data.List (sortOn,groupBy,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>))
import Data.String
import Data.Foldable (toList)

-- | Annotated string, which consists of segments with separate (or no) annotations.
--
-- We keep annotated segments in a container (list).
-- The annotation is @Maybe a@, because the no-annotation case is common.
--
-- /Note:/ with @Last x@ annotation, the 'annotate' will overwrite all annotations.
--
-- /Note:/ if the list is changed into `Seq` or similar structure
-- allowing fast viewr and viewl, then we can impose an additional
-- invariant that there aren't two consequtive non-annotated segments;
-- yet there is no performance reason to do so.
--
data AS a = AS !Int [(a, String)]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- | Tests the invariants of 'AS'
_validAs :: AS a -> Bool
_validAs (AS i s) = lengthInvariant && noNewlineInvariant
  where
    lengthInvariant = i == sum (map (length . snd) s)
    noNewlineInvariant = all (notElem '\n' . snd) s

asLength :: AS a -> Int
asLength (AS l _) = l

-- | Make a non-annotated 'AS'.
mkAS :: Monoid a => String -> AS a
mkAS s = AS (length s) [(mempty, s)]

instance Semigroup (AS a) where
  AS i xs <> AS j ys = AS (i + j) (xs <> ys)

newtype L a = L (Seq (AS a)) -- non-empty sequence
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Monoid a => Semigroup (L a) where
  L (viewr -> xs :> x) <> L (viewl -> y :< ys) = L (xs <> singleton (x <> y) <> fmap (indent <>) ys)
      where n = asLength x
            indent = mkAS (Prelude.replicate n ' ')
  L _ <> L _ = error "<> @L: invariant violated, Seq is empty"

instance Monoid a => Monoid (L a) where
   mempty = L (singleton (mkAS ""))
   mappend = (<>)

instance Monoid a => Layout (L a) where
   text = L . singleton . mkAS
   flush (L xs) = L (xs |> mkAS "")

instance Render L where
  renderWith f (L xs) = intercalate (toList xs)
    where
      f' (AS _ s) = foldMap (uncurry f) s
      sep = f mempty "\n"

      intercalate []     = mempty
      intercalate (y:ys) = f' y `mappend` foldMap (mappend sep . f') ys

-- | This class is split from 'Layout', because of different
-- kinds: @Render :: (* -> *) -> Constraint@ and @Layout :: * -> Constraint@.
class Render d where
  renderWith :: (Monoid r,  Monoid a)
             => (a -> String -> r) -- ^ how to annotate the string. /Note:/ the annotation should preserve the visible length of the string.
             -> d a                -- ^ renderable
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

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn m = go
  where
    go [] xs = xs
    go xs [] = xs
    go (x:xs) (y:ys)
      | m x <= m y  = x:go xs (y:ys)
      | otherwise    = y:go (x:xs) ys

mergeAllOn :: Ord b => (a -> b) -> [[a]] -> [a]
mergeAllOn _ [] = []
mergeAllOn m (x:xs) = mergeOn m x (mergeAllOn m xs)

bestsOn :: forall a b. (Poset b, Ord b)
      => (a -> b) -- ^ measure
      -> [[a]] -> [a]
bestsOn m = paretoOn' m [] . mergeAllOn m

-- | @paretoOn m = paretoOn' m []@
paretoOn' :: Poset b => (a -> b) -> [a] -> [a] -> [a]
paretoOn' m acc [] = Prelude.reverse acc
paretoOn' m acc (x:xs) = if any ((≺ m x) . m) acc
                            then paretoOn' m acc xs
                            else paretoOn' m (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.

newtype Doc a = MkDoc [(M a,L a)] -- list sorted by lexicographic order for the first component
  deriving Show

instance Monoid a => Semigroup (Doc a) where
  MkDoc xs <> MkDoc ys = MkDoc $ bestsOn fst [ quasifilter (fits . fst) [x <> y | y <- ys] | x <- xs]
    where quasifilter p zs = let fzs = filter p zs
                             in if null fzs
                                then [minimumBy (compare `on` (maxWidth . fst)) zs]
                                else fzs

instance Monoid a => Monoid (Doc a) where
  mempty = text ""
  mappend = (<>)

-- TODO: make columns configurable
fits :: M a -> Bool
fits x = maxWidth x <= 80

instance Monoid a => Layout (Doc a) where
  flush (MkDoc xs) = MkDoc $ bestsOn fst $ map (sortOn fst) $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = paretoOn' fst [] $ sort $ (map flush xs)
  text s = MkDoc [text s]

instance Render Doc where
  renderWith _ (MkDoc []) = error "No suitable layout found."
  renderWith f (MkDoc xs@(x:_)) | maxWidth (fst x) <= 80 = renderWith f (snd x)
                            | otherwise = renderWith f $ snd (minimumBy (compare `on` (maxWidth . fst)) xs)

instance Monoid a => Document (Doc a) where
  MkDoc m1 <|> MkDoc m2 = MkDoc (bestsOn fst [m1,m2])


instance (Layout a, Layout b) => Layout (a,b) where
  text s = (text s, text s)
  flush (a,b) = (flush a, flush b)

instance (Document a, Document b) => Document (a,b) where
  (a,b) <|> (c,d) = (a<|>c,b<|>d)

instance Monoid a => IsString (Doc a) where
  fromString = text

-- | `<>` new annotation to the 'Doc'.
--
-- Example: 'Any True' annotation will transform the rendered 'Doc' into uppercase:
--
-- >>> let r = putStrLn . renderWith (\a x -> if a == Any True then map toUpper x else x)
-- >>> r $ text "hello" <$$> annotate (Any True) (text "world")
-- hello
-- WORLD
--
annotate :: forall a. Monoid a => a -> Doc a -> Doc a
annotate a (MkDoc xs) = MkDoc (fmap (fmap annotateL) xs)
  where
    annotateL :: L a -> L a
    annotateL (L s) = L (fmap annotateAS s)

    annotateAS :: AS a -> AS a
    annotateAS (AS i s) = AS i (fmap annotatePart s)

    annotatePart (b, s) = (b `mappend` a, s)

    ma = Just a

-- $setup
-- >>> import Text.PrettyPrint.Compact
-- >>> import Data.Monoid
-- >>> import Data.Char
