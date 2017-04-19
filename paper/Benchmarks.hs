{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Criterion (nf)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Types (DataRecord(..), Report(..), SampleAnalysis(..), Benchmarkable)
import PM (render, L, M(..), DM(..), pretty, SExpr(..))
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Random
import qualified Criterion.Main.Options as C
import qualified Criterion.Monad as C

data OC = Open | Close

genRandomOC :: Int -> IO [OC]
genRandomOC maxLen = go 0 0
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

randExpr maxlen = do
  oc <- genRandomOC maxlen
  return $ SExpr $ snd $ runOCP ocToSExprs oc

testLayout :: SExpr -> Int
testLayout input = height mm
    where mm :: M
          mm = minimum $ (pretty input :: DM)
          _l :: String
          _l = render $ (pretty input :: [L])

benchmark :: SExpr -> Benchmarkable
benchmark size = nf testLayout size

performanceAnalysisRandom :: IO ()
performanceAnalysisRandom = do
  putStrLn "performanceAnalysisRandom..."
  exprs <- forM [1..10] $ \_ -> randExpr 10000
  putStrLn "If the program gets stuck now it is due to a bug in criterion. (It does not work on MacOS)"
  an <- C.withConfig C.defaultConfig $ do
    forM (zip exprs [1..]) $ \(e,i) -> do
      liftIO $ putStrLn $ "running bench " ++ show i
      (Analysed (Report { reportAnalysis = SampleAnalysis {anMean = dt}})) <-
         runAndAnalyseOne i ("bench " ++ show i) (benchmark e)
      return (testLayout e, dt)
  writeFile "benchmark-random.dat" $ show an

main = do
  performanceAnalysisRandom

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

  
