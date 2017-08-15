{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}
module Text.PrettyPrint.Compact.Core(Annotation,Layout(..),renderWith,Options(..),groupingBy,Doc) where

import Prelude ()
import Prelude.Compat as P

import Data.List.Compat (sortOn,groupBy,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>))
import Data.String
import Data.Foldable (toList)
import Control.Applicative (liftA2)
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
            indent = mkAS (P.replicate n ' ')
  L _ <> L _ = error "<> @L: invariant violated, Seq is empty"

instance Monoid a => Monoid (L a) where
   mempty = L (singleton (mkAS ""))
   mappend = (<>)

instance Layout L where
   text = L . singleton . mkAS
   flush (L xs) = L (xs |> mkAS "")
   annotate a (L s') = L (fmap annotateAS s')
      where annotateAS (AS i s) = AS i (fmap annotatePart s)
            annotatePart (b, s) = (b `mappend` a, s)


renderWithL :: (Monoid a, Monoid r) => Options a r -> L a -> r
renderWithL opts (L xs) = intercalate (toList xs)
  where
    f = optsAnnotate opts
    f' (AS _ s) = foldMap (uncurry f) s
    sep = f mempty "\n"

    intercalate []     = mempty
    intercalate (y:ys) = f' y `mappend` foldMap (mappend sep . f') ys

data Options a r = Options
    { optsPageWidth :: !Int              -- ^ maximum page width
    , optsAnnotate  :: a -> String -> r  -- ^ how to annotate the string. /Note:/ the annotation should preserve the visible length of the string.
    }

class Layout d where
  text :: Monoid a => String -> d a
  flush :: Monoid a => d a -> d a
  -- | `<>` new annotation to the 'Doc'.
  --
  -- Example: 'Any True' annotation will transform the rendered 'Doc' into uppercase:
  --
  -- >>> let r = putStrLn . renderWith defaultOptions { optsAnnotate = \a x -> if a == Any True then map toUpper x else x }
  -- >>> r $ text "hello" <$$> annotate (Any True) (text "world")
  -- hello
  -- WORLD
  --
  annotate :: forall a. Monoid a => a -> d a -> d a

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

instance Monoid a => Monoid (M a) where
  mempty = text ""
  mappend = (<>)

instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  annotate _ M{..} = M{..}
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
paretoOn' _ acc [] = P.reverse acc
paretoOn' m acc (x:xs) = if any ((≺ m x) . m) acc
                            then paretoOn' m acc xs
                            else paretoOn' m (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.

-- list sorted by lexicographic order for the first component
-- function argument is the page width
newtype ODoc a = MkDoc {fromDoc :: Int -> [(Pair M L a)]}

instance Monoid a => Semigroup (ODoc a) where
  MkDoc xs <> MkDoc ys = MkDoc $ \w -> bestsOn frst [ discardInvalid w [x <> y | y <- ys w] | x <- xs w]

discardInvalid w = quasifilter (fits w . frst)

quasifilter _ [] = []
quasifilter p zs = let fzs = filter p zs
                   in if null fzs -- in case that there are no valid layouts, we take a narrow one.
                      then [minimumBy (compare `on` (maxWidth . frst)) zs]
                      else fzs

instance Monoid a => Monoid (ODoc a) where
  mempty = text ""
  mappend = (<>)

-- TODO: make columns configurable
fits :: Int -> M a -> Bool
fits w x = maxWidth x <= w

instance Layout ODoc where
  text s = MkDoc $ \_ -> [text s]
  flush (MkDoc xs) = MkDoc $ \w -> fmap flush (xs w)
  annotate a (MkDoc xs) = MkDoc $ \w -> fmap (annotate a) (xs w)

renderWith :: (Monoid r, Annotation a)
           => Options a r  -- ^ rendering options
           -> ODoc a          -- ^ renderable
           -> r
renderWith opts d = case xs of
    [] -> error "No suitable layout found."
    ((_ :-: x):_) -> renderWithL opts x
  where
    pageWidth = optsPageWidth opts
    xs = discardInvalid pageWidth (fromDoc d pageWidth)

onlySingleLine :: [Pair M L a] -> [Pair M L a]
onlySingleLine = takeWhile (\(M{..} :-: _) -> height == 0)

spaces :: (Monoid a,Layout l) => Int -> l a
spaces n = text $ replicate n ' '

($$) :: (Layout d, Monoid a, Semigroup (d a)) =>
        d a -> d a -> d a
a $$ b = flush a <> b

second f (a,b) = (a, f b)

groupingBy :: Monoid a => String -> [(Int,Doc a)] -> Doc a
groupingBy _ [] = mempty
groupingBy separator ms = MkDoc $ \w ->
  let mws = map (second (($ w) . fromDoc)) ms
      (_,lastMw) = last mws
      hcatElems = map (onlySingleLine . snd) (init mws) ++ [lastMw] -- all the elements except the first must fit on a single line
      vcatElems = map (\(indent,x) -> map (spaces indent <>) x) mws
      horizontal = discardInvalid w $ foldr1 (liftA2 (\x y -> x <> text separator <> y)) hcatElems
      vertical = foldr1 (\xs ys -> bestsOn frst [[x $$ y | y <- ys] | x <- xs]) vcatElems
  in bestsOn frst [horizontal,vertical]

data Pair f g a = (:-:) {frst :: f a, scnd :: g a}

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Pair f g a) where
  (x :-: y) <> (x' :-: y') = (x <> x') :-: (y <> y')
instance (Monoid (f a), Monoid (g a)) => Monoid (Pair f g a) where
  mempty = mempty :-: mempty
  mappend (x :-: y)(x' :-: y') = (x `mappend` x') :-: (y `mappend` y')

instance (Layout a, Layout b) => Layout (Pair a b) where
  text s = text s :-: text s
  flush (a:-:b) = (flush a:-: flush b)
  annotate x (a:-:b) = (annotate x a:-:annotate x b)

instance Monoid a => IsString (Doc a) where
  fromString = text

-- data DDoc a = Text String | Flush (DDoc a) | S (Seq (DDoc a)) | DDoc a :<|> DDoc a | Fail | Annotate a (DDoc a)
--   deriving Eq

type Annotation a = (Monoid a)

-- interp :: Annotation a => DDoc a -> ODoc a
-- interp = \case
--   Text s -> text s
--   Flush d -> flush (interp d)
--   Fail -> empty
--   S ds -> foldMap interp $ catTexts $ toList ds
--   d :<|> e -> interp d <|> interp e
--   Annotate a d -> annotate a (interp d)


-- catTexts :: forall a. [DDoc a] -> [DDoc a]
-- catTexts (Text t:Text u:xs) = catTexts (Text (t<>u):xs)
-- catTexts (x:xs) = x:catTexts xs
-- catTexts [] = []

-- instance Semigroup (DDoc a) where
--   Fail <> _ = Fail
--   _ <> Fail = Fail
--   S as <> S bs = S (as <> bs)
--   S as <> b = S (as <> singleton b)
--   a <> S bs = S (singleton a <> bs)
--   a <> b = S (singleton a <> singleton b)

-- instance Monoid a => Monoid (DDoc a) where
--   mempty = text ""
--   mappend = (<>)

-- instance Layout DDoc where
--   text = Text
--   flush (Flush x) = Flush x
--   flush x = Flush x
--   annotate = Annotate -- can be pushed down to text. Is this a good idea? (Note it'll get rid of complications in the classes, etc.)

-- instance Document DDoc where
--   -- S (viewl -> a :< as) <|> S (viewl -> b :< bs) | a == b = a <> (S as <|> S bs)
--   -- S (viewr -> as :> a) <|> S (viewr -> bs :> b) | a == b = (S as <|> S bs) <> a
--   Flush a <|> Flush b = Flush (a <|> b)
--   Annotate a b <|> Annotate a' d | a == a' = Annotate a' (b <|> d)
--   a <|> b = a <||> b
--   empty = Fail

-- (<||>) :: forall a. DDoc a -> DDoc a -> DDoc a
-- a <||> Fail = a
-- Fail <||> a = a
-- a <||> b = a :<|> b


type Doc = ODoc

tt :: Doc ()
tt = groupingBy " " $ map (4,) $ 
     ((replicate 4 $ groupingBy " " (map (4,) (map text ["fw"]))) ++
      [groupingBy " " (map (0,) (map text ["fw","arstnwfyut","arstin","arstaruf"]))])

{-> putStrLn (renderWith Options { optsAnnotate = \_ s -> s , optsPageWidth = 4} tt)

    fw
    fw
    fw
    fw
    fw
    arstnwfyut
    arstin
    arstaruf
-}

-- $setup
-- >>> import Text.PrettyPrint.Compact
-- >>> import Data.Monoid
-- >>> import Data.Char
