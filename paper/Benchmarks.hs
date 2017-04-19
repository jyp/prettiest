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
import Data.Maybe
import Data.List

data OC = Open | Close | A

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
    Just A -> do
      skip
      rest <- ocToSExprs
      return (Atom "a":rest)
    Just Open -> do
      h <- ocToSExpr
      rest <- ocToSExprs
      return (h:rest)
    _ -> return []


randExpr :: Int -> IO SExpr
randExpr maxlen = do
  oc <- genRandomOC maxlen
  return $ SExpr $ snd $ runOCP ocToSExprs $ intersperse A oc

testLayout :: SExpr -> Maybe Int
testLayout input = case (pretty input :: DM) of
                     [] -> Nothing
                     mm -> Just $ height $ minimum mm

benchmark :: SExpr -> Benchmarkable
benchmark size = nf testLayout size

fitting :: SExpr -> Bool
fitting = isJust . testLayout 

performanceAnalysisRandom :: IO ()
performanceAnalysisRandom = do
  putStrLn "performanceAnalysisRandom..."
  exprs <- filter fitting <$> forM [(1::Int)..10] (\_ -> randExpr 10000)
  putStrLn "If the program gets stuck now it is due to a bug in criterion. (It does not work on MacOS)"
  an <- C.withConfig C.defaultConfig $ do
    forM (zip exprs [1..]) $ \(e,i) -> do
      liftIO $ putStrLn $ "running bench " ++ show i
      (Analysed (Report { reportAnalysis = SampleAnalysis {anMean = dt}})) <-
         runAndAnalyseOne i ("bench " ++ show i) (benchmark e)
      return (testLayout e, dt)
  writeFile "benchmark-random.dat" $ show an

main :: IO ()
main = do
  performanceAnalysisRandom

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal repl")
-- End:

  
