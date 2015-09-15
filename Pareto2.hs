{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies, DataKinds, ConstraintKinds, PolyKinds, TypeOperators, MagicHash, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Pareto2 where

import Data.Monoid

class Ord (Coord p) => Point p where
  type Coord p
  dims :: p -> Int
  coord :: Int -> p -> Coord p

data KdTree p where
  Empty :: KdTree p
  Node :: !Int -> KdTree p -> Either p (Coord p) -> KdTree p -> KdTree p

instance (Show p, Show (Coord p)) => Show (KdTree p) where
  show Empty = "()"
  show (Node d l x r) = "(Node " ++ show  d ++ " " ++ show l ++ " " ++ show x ++ " " ++ show r ++ ")"

-- equal things go to the left; so on the right nothing dominates p

instance Foldable KdTree where
  foldMap _ Empty = mempty
  foldMap f (Node _ l x r) = foldMap f l <> a <> foldMap f r
    where a = case x of
            Left y -> f y
            Right _ -> mempty

instance Ord a => Point (a,a,a) where
  type Coord (a,a,a) = a
  dims _ = 3
  coord 0 (x,_,_) = x
  coord 1 (_,x,_) = x
  coord 2 (_,_,x) = x

instance Ord a => Poset (a,a,a) where
  (x,y,z) ≺ (x',y',z') = x <= x' && y <= y' && z <= z'

class Poset a where
  (≺) :: a -> a -> Bool

pareto :: (Point p, Poset p) => [p] -> KdTree p
pareto = foldr (insert 0) Empty

insert :: (Poset p, Point p) => Int -> p -> KdTree p -> KdTree p
insert d p' Empty = Node (d `mod` dims p') Empty (Left p') Empty
insert _ p' (Node d l p r) = case p of
          Left q | q ≺ p' -> Node d l' p r'
                 | p' ≺ q -> Node d lp' (Right (coord d q)) r'
          _ -> if coord d p' <= coord' d p
                                  then Node d lp' p r'
                                  else Node d l' p (insert (d+1) p' r)
    where l' = filterOut bs l
          lp' = insert (d+1) p' l
          r' = filterOut bs r
          bs = coords p'

coords :: forall p. Point p => p -> [(Int, Coord p)]
coords p = [(d,coord d p) | d <- [0..dims p-1]]

coord' :: forall p. Point p => Int -> Either p (Coord p) -> Coord p
coord' d (Left p) = coord d p
coord' _ (Right x) = x

filterOut :: (c ~ Coord p, Ord c, Point p) => [(Int,c)] -> KdTree p -> KdTree p
filterOut [] t = t
filterOut bs t = case t of
                  Empty -> Empty
                  Node d l p r ->
                    case lookup d bs of
                      Nothing -> Node d (filterOut bs l) p r'
                      Just x -> if coord' d p < x
                                   then Node d l p (filterOut bs r)
                                   else if keep
                                        then Node d (filterOut bs l) p r'
                                        else Node d (filterOut bs l) (Right $ coord' d p) r'
                     where bs' = filter (not . (== d) . fst) bs
                           r' = filterOut bs' r
                           keep = not $ and [y <= coord' d p | (d,y) <- bs]


test = pareto ([(10,10,10)] :: [(Int,Int,Int)])

test' = insert 0 (1,1,1) test
test'' = insert 0 (100,100,100) test
