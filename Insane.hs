{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Insane where

import Data.List (intercalate,sort)

empty :: Layout d => d
empty = text ""

(<+>) :: Layout d => d -> d -> d
x <+> y = x <-> text " " <-> y

hsep,vcat :: DOC d => [d] -> d
vcat [] = empty
vcat xs = foldr1 ($$) xs
hsep [] = empty
hsep xs = foldr1 (<+>) xs

sep :: DOC d => [d] -> d
sep [] = empty
sep xs = hsep xs <|> vcat xs

type L = [String] -- non empty.

instance Monoid Doc where
  mempty = empty
  mappend = (<->)

instance Layout L where
   render = intercalate "\n"

   text s = [s]

   xs <-> (y:ys) = xs0 ++ [x ++ y] ++ map (indent ++) ys
      where xs0 = init xs
            x = last xs
            n = length x
            indent = replicate n ' '

   flush xs = xs ++ [""]

($$) :: forall d. Layout d => d -> d -> d
a $$ b = flush a <-> b

class Layout d where
  (<->) :: d -> d -> d
  text :: String -> d
  flush :: d -> d
  render :: d -> String



class Layout d => DOC d where
  (<|>) :: d -> d -> d


data M = M {height    :: Int,
            lastWidth :: Int,
            maxWidth  :: Int
            }
  deriving (Show,Eq,Ord)

instance Layout M where
  text s = M {height = 0, maxWidth = length s, lastWidth = length s}
  a <-> b = M {maxWidth = max (maxWidth a) (maxWidth b + lastWidth a),
              height = height a + height b,
              lastWidth = lastWidth a + lastWidth b}
  flush a = M {maxWidth = maxWidth a,
               height = height a + 1,
               lastWidth = 0}
  render = error "don't use this render"

fits :: M -> Bool
fits x = maxWidth x <= 80

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
pareto' acc [] = reverse acc
pareto' acc (x:xs) = if any (≺ x) acc
                       then pareto' acc xs
                       else pareto' (x:acc) xs

data Doc = Doc
              {minW :: Int, -- min width
              minLW :: Int, -- min last width
              doc0 :: Int -> -- available width
                      Int -> -- available last width (invar: less than the above.)
                      [(M,L)]}

instance Layout Doc where
  Doc m1 w1 xs1 <-> Doc m2 w2 xs2 = Doc (max m1 (w1 + m2))
                                    (w1 + w2)
                                    (\w lw ->
                                          let xs = xs1 w (w - m2)
                                              ys = xs2 (w - w1) (min lw (w - w1))
                                              val (a,_) = (maxWidth a <= w) && (lastWidth a <= lw)
                                          in bests [filter val [x <-> y | y <- ys] | x <- xs])
  text t = Doc (length t) (length t) (\w _lw -> [text t | length t <= w])
  flush (Doc m _ xs1) = Doc m 0 (\w _lw -> pareto' [] (sort (map flush (xs1 w w))))
     -- TODO: is sort needed?
  render (Doc _ _ x) = case (x 80 80) of
    [] -> "overflow!"
    l:_ -> render l


instance DOC Doc where
  Doc m1 w1 x1 <|> Doc m2 w2 x2 = Doc (min m1 m2) (min w1 w2) (\w lw -> x1 w lw ++ x2 w lw)

hang :: DOC d => Int -> d -> d -> d
hang n x y = (x <+> y) <|> (x $$ nest' n y)

nest' :: Layout d => Int -> d -> d
nest' n y = spaces n <-> y

spaces :: Layout d => Int -> d
spaces n = text $ replicate n ' '


instance (Layout a, Layout b) => Layout (a,b) where
  text s = (text s, text s)
  flush (a,b) = (flush a, flush b)
  (a,b) <-> (c,d) = (a<->c ,b<->d)
  render = render . snd

instance (DOC a, DOC b) => DOC (a,b) where
  (a,b) <|> (c,d) = (a<|>c,b<|>d)

instance (Poset a) => Poset (a,b) where
  (a,_) ≺ (b,_) = a ≺ b

