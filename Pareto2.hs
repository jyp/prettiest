{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies, DataKinds, ConstraintKinds, PolyKinds, TypeOperators, MagicHash, FlexibleInstances, FlexibleContexts #-}

module Pareto2 where


class Ord (Coord p) => Point p where
  type Coord p
  dims :: p -> Int
  coord :: Int -> p -> Coord p
  update :: Coord p -> p -> p

data KdTree p where
  Empty :: KdTree p
  Node :: Int -> KdTree p -> p -> KdTree p -> KdTree p
-- equal things go to the left; so on the right nothing dominates p

class Poset a where
  (≺) :: a -> a -> Bool

insert :: (Poset p, Point p) => Int -> p -> KdTree p -> KdTree p
insert d p' Empty = Node (d `mod` dims p') Empty p' Empty
insert _ p' (Node d l p r) = if p' ≺ p
                             then Node d l' p r'
                             else if coord d p' < coord d p
                                  then Node d (insert (d+1) p' l) p r'
                                  else Node d l' p (insert (d+1) p' r)
    where l' = filterOut bs l
          r' = filterOut bs r
          bs = [(d',coord d p') | d' <- [0..dims p'-1]]

filterOut :: (c ~ Coord p, Ord c, Point p) => [(Int,c)] -> KdTree p -> KdTree p
-- When bounds are all Nothing, quit.
filterOut [] t = t
filterOut bs t = case t of
                  Empty -> Empty
                  Node d l p r ->
                    case lookup d bs of
                      Nothing -> Node d (filterOut bs l) p r'
                      Just x -> if coord d p < x
                                   then Node d l p (filterOut bs r)
                                   else Node d (filterOut bs l) p r' 
                     where bs' = filter (not . (== d) . fst) bs
                           r' = filterOut bs' r

-- -- Filter everything which is 
-- filterCoord :: Int -> c -> KdTree p -> KdTree p
-- filterCoord d x Empty = Empty
-- filterCoord d x (Node d' l p r) =
--   if x < coord d p
--   then l
--   else filterCoord 
 
