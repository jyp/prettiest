{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ViewPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Text.PrettyPrint.Compact.Core(Layout(..),Document(..),Doc) where

import Data.List (sort,groupBy,intercalate,minimumBy)
import Data.Function (on)
import Data.Semigroup
import Data.Sequence (singleton, Seq, viewl, viewr, ViewL(..), ViewR(..), (|>))
import Data.String
import Data.Foldable (toList)

-- | Annotated string
data AS a = AS (Maybe a) String
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

getASString :: AS a -> String
getASString (AS _ s) = s

-- | Make non-annotated 'AS'
mkAS :: String -> AS a
mkAS = AS Nothing

instance Semigroup a => Semigroup (AS a) where
  AS a s <> AS b t = AS (f a b) (s <> t)
    where
      f (Just x) (Just y) = Just (x <> y)
      f x        Nothing  = x
      f Nothing  y        = y 

newtype L a = L (Seq (AS a)) -- non-empty sequence
  deriving (Eq,Ord,Show,Functor)

instance Semigroup a => Semigroup (L a) where
   L (viewr -> xs :> x) <> L (viewl -> y :< ys) = L (xs <> singleton (x <> y) <> fmap (indent <>) ys)
      where n = length (getASString x)
            indent = mkAS (Prelude.replicate n ' ')

instance Semigroup a =>  Monoid (L a) where
   mempty = L (singleton (mkAS ""))
   mappend = (<>)

instance Semigroup a => Layout (L a) where
   render (L xs) = intercalate "\n" $ map getASString $ toList xs
   text = L . singleton . mkAS
   flush (L xs) = L (xs |> mkAS "")


class Monoid d => Layout d where
  text :: String -> d
  flush :: d -> d
  render :: d -> String

class Layout d => Document d where
  (<|>) :: d -> d -> d

data M = M {height    :: Int,
            lastWidth :: Int,
            maxWidth  :: Int
            }
  deriving (Show,Eq,Ord)

instance Semigroup M where
  a <> b =
    M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
       height = height a + height b,
       lastWidth = lastWidth a + lastWidth b}
  
instance Monoid M where
  mempty = text ""
  mappend = (<>)

instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  render = error "don't use render for M type"

class Poset a where
  (≺) :: a -> a -> Bool


instance Poset M where
  M c1 l1 s1 ≺ M c2 l2 s2 = c1 <= c2 && l1 <= l2 && s1 <= s2

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys


mergeAll :: Ord a => [[a]] -> [a]
mergeAll [] = []
mergeAll (x:xs) = merge x (mergeAll xs)

bests :: forall a. (Ord a, Poset a) => [[a]] -> [a]
bests = pareto' [] . mergeAll

pareto' :: Poset a => [a] -> [a] -> [a]
pareto' acc [] = Prelude.reverse acc
pareto' acc (x:xs) = if any (≺ x) acc
                       then pareto' acc xs
                       else pareto' (x:acc) xs
                            -- because of the ordering, we have that
                            -- for all y ∈ acc, y <= x, and thus x ≺ y
                            -- is false. No need to refilter acc.


newtype Doc a = MkDoc [(M,L a)] -- list sorted by lexicographic order for the first component
  deriving Show

instance (Ord a, Semigroup a) => Semigroup (Doc a) where
  MkDoc xs <> MkDoc ys = MkDoc $ bests [ quasifilter (fits . fst) [x <> y | y <- ys] | x <- xs]
    where quasifilter p xs = let fxs = filter p xs
                             in if null fxs
                                then [minimumBy (compare `on` (maxWidth . fst)) xs]
                                else fxs

instance (Ord a, Semigroup a) => Monoid (Doc a) where
  mempty = text ""
  mappend = (<>)

fits :: M -> Bool
fits x = maxWidth x <= 80

instance (Ord a, Semigroup a) => Layout (Doc a) where
  flush (MkDoc xs) = MkDoc $ bests $ map sort $ groupBy ((==) `on` (height . fst)) $ (map flush xs)
  -- flush xs = pareto' [] $ sort $ (map flush xs)
  text s = MkDoc [text s]
  render (MkDoc []) = error "No suitable layout found."
  render (MkDoc xs@(x:_)) | maxWidth (fst x) <= 80 = render x
                          | otherwise = render (minimumBy (compare `on` (maxWidth . fst)) xs)

instance (Ord a, Semigroup a) => Document (Doc a) where
  MkDoc m1 <|> MkDoc m2 = MkDoc (bests [m1,m2])


instance (Layout a, Layout b) => Layout (a,b) where
  text s = (text s, text s)
  flush (a,b) = (flush a, flush b)
  render = render . snd

instance (Document a, Document b) => Document (a,b) where
  (a,b) <|> (c,d) = (a<|>c,b<|>d)

instance (Poset a) => Poset (a,b) where
  (a,_) ≺ (b,_) = a ≺ b

instance (Ord a, Semigroup a) =>  IsString (Doc a) where
  fromString = text
