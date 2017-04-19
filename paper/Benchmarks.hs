{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

import System.Random
import Control.Monad (ap)
import PM (render, L, M, pretty, SExpr(..))

data OC = Open | Close


genRandomExpr :: Int -> IO [OC]
genRandomExpr maxLen = go 0 0
  where
    go :: Int -> Int -> IO [OC]
    go opened closed
      | closed >= maxLen = return []
      | opened >= maxLen = close
      | closed >= opened = open
      | otherwise = do
          b <- randomIO
          if b then open else close
      where open  = (Open: ) <$> go (1+opened) closed
            close = (Close:) <$> go opened (1+closed)

newtype OCP a = OCP {runOCP :: [OC] -> ([OC],a)} deriving (Functor)
instance Applicative OCP where
  pure = return
  (<*>) = ap
instance Monad OCP where
  return x = OCP (\xs -> (xs,x))
  OCP k >>= f = OCP (\i0 -> let (i1,x) = k i0 in runOCP (f x) i1)

look :: OCP (Maybe OC)
look = OCP $ \case
  [] -> ([],Nothing)
  (x:xs) -> (x:xs,Just x)

skip :: OCP ()
skip = OCP $ \case
  [] -> ([],())
  (_:xs) -> (xs,())

ocToSExpr :: OCP SExpr
ocToSExpr = do
  skip -- open assumed
  x <- ocToSExprs
  skip -- close or eof
  return (SExpr x)

ocToSExprs :: OCP [SExpr]
ocToSExprs = do
  l <- look
  case l of
    Just Open -> do
      h <- ocToSExpr
      rest <- ocToSExprs
      return (h:rest)
    _ -> return [Atom "a"]

randExpr :: IO ()
randExpr = do
  oc <- genRandomExpr 500
  let expr = snd $ runOCP ocToSExprs oc
  putStrLn $ render $ (pretty (SExpr expr) :: [(M,L)])

{-> randExpr

((((((a) (a) ((a) a) ((a) a) a)
    ((a) ((((a) a) (a) a)
          (((a) (a) ((a) a) a)
           ((a) ((a) (((a) a) (a) a) (a) (((a) ((a) a) ((a) a) a) (a) a) a) a)
           a)
          (a)
          (((((a) (a) (a) a) a) ((((a) a) (((a) (a) (a) a) a) a)
                                 (a)
                                 a) (a) ((a) (a) a) ((a) (a) a) (a) a) (a) a)
          a) a)
    a) (a) a) a)
 ((((((a) a) a) a) a) (a) a)
 ((a) a)
 (a)
 (((a) (a) a) (a) a)
 ((a) (((a) a) (a) ((a) (a) a) (a) a) a)
 (((a) ((a) a) a) ((((a) a) a) a) (a) (a) a)
 ((a) ((a) (a) (a) a) (a) a)
 (a)
 (a)
 ((a) (a) (a) ((a) a) a)
 (a)
 ((a) a)
 ((a) (a) ((a) a) a)
 (a)
 (((a) a) (a) (a) (a) ((((((a) (a) (((((a) (a) a) (a) (a) (a) a) a)
                                    a) (a) a) (a) ((a) (a) (a) a) (a) (a) a)
                         a) (a) (a) a) (a) (a) a) a)
 ((a) a)
 (a)
 ((a) (((a) a) a) a)
 (a)
 (a)
 (a)
 (a)
 (a)
 (a)
 (a)
 ((a) a)
 (a)
 (((a) (((a) a) a) (a) a) (a) a)
 ((((a) ((a) a) a) a) a)
 (a)
 (a)
 ((a) (((a) ((a) a) a) a) (a) a)
 (((a) a) (a) ((((a) a) (a) a) a) ((a) a) (a) a)
 (a)
 ((a) ((a) ((a) a) (a) a) (a) (a) ((a) a) a)
 ((a) a)
 (a)
 ((a) a)
 (a)
 ((a) (a) a)
 ((((a) ((a) a) a) a) a)
 ((a) a)
 (((a) (a) a)
  (((((a) (((((a) ((((a) a)
                    (a)
                    ((a) a)
                    (a)
                    (a)
                    ((a) a)
                    ((a) (((((a) (a) (a) ((a) a) ((a) (a) ((((a) a) a)
                                                           (a)
                                                           a) (((a) a)
                                                               a) a) (a) a)
                            (a)
                            a) (((a) (a) a) a) a) a) (a) ((a) a) (a) (a) a)
                    ((a) a)
                    ((a) (a) ((a) ((a) a) a) a)
                    a) a) (((a) (a) a) a) (a) ((a) a) a)
             (((a) a) (a) (a) ((a) a) a)
             a) (a) (a) ((a) a) a) (((a) ((a) ((a) (a) a) a) (a) a)
                                    a) (a) (((a) a) a) a) a) (a) a) (a) a) a)
  (a)
  a)
 ((a) (((((a) a) a) (a) ((a) a) (((a) ((a) a) a) a) (a) a) a) a)
 ((((a) ((a) (a) (a) (a) a) ((((a) a) (a) a) a) a)
   ((a) (((a) a) (a) a) ((a) a) ((a) a) (a) (a) a)
   ((((((a) (((a) (a) ((a) (a) a) a) (a) (a) a) ((a) ((a) a) a) a)
       (((a) a) a)
       a) a) ((((a) a) a) a) a) a)
   a) (((((a) a) (((a) a) (a) ((a) (a) a) a) a)
        ((((((a) (a) a)
            a) (a) a) (a) ((((a) a) a) a) ((a) (a) a) (a) (a) (a) ((a) a) a) a)
        a) a) (a) a)
 (((a) (a) (a) (a) a) (a) (a) a)
 ((a) a)
 (a)
 ((a) ((a) a) (a) (((a) (a) (a) a)
                   (a)
                   ((a) a)
                   (a)
                   (((a) ((a) a) (a) a) a)
                   ((a) (a) a)
                   ((a) a)
                   ((((a) a) a) (((a) (((((a) ((a) a) a) a) (a) a)
                                       a) a) a) (a) a)
                   a) a)
 a)
-}

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal repl")
-- End:

  
