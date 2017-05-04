{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Criterion (nf)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Types (DataRecord(..), Report(..), SampleAnalysis(..), Benchmarkable)
import PM (render, L, M(..), DM(..), NoDom(..), pretty, SExpr(..), dataFileName, testExpr, OC(..), randomDyck)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Random
import qualified Criterion.Main.Options as C
import qualified Criterion.Monad as C
import Data.Maybe
import Data.List
import System.Environment (getArgs)
import BenchmarkLibs
import qualified BenchmarkXML as XML

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
  oc <- randomDyck maxlen
  return $ SExpr $ snd $ runOCP ocToSExprs $ intersperse A oc

testLayout :: SExpr -> Maybe Int
testLayout input = case (pretty input :: DM) of
                     [] -> Nothing
                     mm -> Just (1 + (height (minimum mm)))

testLayoutNoDom :: SExpr -> Maybe Int
testLayoutNoDom input = case (pretty input :: NoDom) of
                     NoDom [] -> Nothing
                     NoDom mm -> Just (1 + (height (minimum mm)))

benchmark testLayout size = nf testLayout size

fitting :: SExpr -> Bool
fitting = isJust . testLayout

performanceAnalysisRandom :: IO ()
performanceAnalysisRandom = do
  putStrLn "performanceAnalysisRandom..."
  let n = 100 -- number of data points
      maxsz = 4000 -- max number of open parens
      f = exp (log maxsz / n)
  exprs <- filter fitting <$> forM [0..n] (\i -> randExpr (floor (f**i)))
  putStrLn "If the program gets stuck now it is due to a bug in criterion. (It does not work on MacOS)"
  an <- C.withConfig C.defaultConfig $ do
    forM (zip exprs [1..]) $ \(e,i) -> do
      liftIO $ putStrLn $ "running bench " ++ show i
      (Analysed (Report { reportAnalysis = SampleAnalysis {anMean = dt}})) <-
         runAndAnalyseOne i ("bench " ++ show i) (benchmark testLayout e)
      return (i,fromJust (testLayout e), dt)
  writeFile "benchmark-random.dat" $ show an

performanceAnalysis testLayout = do
  putStrLn "performanceAnalysis..."
  putStrLn "If the program gets stuck now it is due to a bug in criterion. (It does not work on MacOS)"
  an <- C.withConfig C.defaultConfig $ do
    forM [1..16] $ \size -> do
      liftIO $ putStrLn $ "running for " ++ show size
      (Analysed (Report { reportAnalysis = SampleAnalysis {anMean = dt}})) <-
         runAndAnalyseOne size ("bench " ++ show size) ((benchmark testLayout . testExpr) size)
      return (size,fromJust (testLayout $ testExpr size), dt)
  writeFile dataFileName $ show an

performanceAnalysisJSON, performanceAnalysisXML :: [FilePath] -> IO ()
performanceAnalysisJSON = performanceAnalysisRW readJSONValue [(pcTest,"PC"), (wlTest,"WL"), (hpjTest,"HPJ")] 
performanceAnalysisXML = performanceAnalysisRW XML.readXMLValue [(XML.pcTest,"PC"), (XML.wlTest,"WL"), (XML.hpjTest,"HPJ")] 

performanceAnalysisRW :: (String -> IO x) -> [(x -> String, String)] -> [FilePath] -> IO ()
performanceAnalysisRW reader tests fnames = do
  putStrLn "performanceAnalysisRW..."
  putStrLn "If the program gets stuck now it is due to a bug in criterion. (It does not work on MacOS)"
  an <- C.withConfig C.defaultConfig $ do
    forM fnames $ \fname -> do
      forM tests $ \(f,name) -> do
        liftIO $ putStrLn $ "running for " ++ name
        j <- liftIO $ reader fname
        (Analysed (Report { reportAnalysis = SampleAnalysis {anMean = dt}})) <-
           runAndAnalyseOne 7 ("bench-" ++ fname ++ name) ((\x -> nf f x) j)
        return (name, dt)
  writeFile "perf.dat" $ show an


main :: IO ()
main = do
  as <- getArgs
  case as of
    ["all"] -> do
      performanceAnalysis testLayout
      performanceAnalysisRandom
      performanceAnalysisJSON ["1k.json","4k.json","10k.json"]
      performanceAnalysisXML ["benchdata/cds.xml"]
    ["full"] -> performanceAnalysis testLayout
    ["nodom"] -> performanceAnalysis testLayoutNoDom
    ["random"] -> performanceAnalysisRandom
    ["json", f] -> performanceAnalysisJSON [f]
    ["xml", f] -> performanceAnalysisXML [f]

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-target: "bench"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal --sandbox-config-file=../cabal.sandbox.config repl --only bench")
-- End:
