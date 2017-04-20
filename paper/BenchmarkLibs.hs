{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

module BenchmarkLibs where

import Control.Monad (ap, forM, forM_)
import Control.Monad.IO.Class
import Criterion (nf)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Types (DataRecord(..), Report(..), SampleAnalysis(..), Benchmarkable)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Random
import qualified Criterion.Main.Options as C
import qualified Criterion.Monad as C
import Data.Maybe
-- import Data.List 
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Text.PrettyPrint.Compact as PC
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import Data.Foldable (toList)
import qualified Text.PrettyPrint.Leijen as WL
import qualified Text.PrettyPrint.HughesPJ as HPJ
import Data.Monoid

prettiestJSON :: Value -> PC.Doc
prettiestJSON (Bool True) = PC.text "true"
prettiestJSON (Bool False) = PC.text "false"
prettiestJSON (Object o) = PC.encloseSep (PC.text "{") (PC.text "}") (PC.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v) = PC.text (show k) PC.<> PC.text ":" PC.<+> prettiestJSON v
prettiestJSON (String s) = PC.string (show s)
prettiestJSON (Array a) =  PC.encloseSep (PC.text "[") (PC.text "]") (PC.text ",") (map prettiestJSON $ toList a)
prettiestJSON Null = PC.mempty
prettiestJSON (Number n) = PC.text (show n)

wlJSON :: Value -> WL.Doc
wlJSON (Bool True) = WL.text "true"
wlJSON (Bool False) = WL.text "false"
wlJSON (Object o) = WL.encloseSep (WL.text "{") (WL.text "}") (WL.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v) = WL.text (show k) WL.<> WL.text ":" WL.<+> wlJSON v
wlJSON (String s) = WL.string (show s)
wlJSON (Array a) =  WL.encloseSep (WL.text "[") (WL.text "]") (WL.text ",") (map wlJSON $ toList a)
wlJSON Null = WL.empty
wlJSON (Number n) = WL.text (show n)

hpjEncloseSep :: HPJ.Doc -> HPJ.Doc -> HPJ.Doc -> [HPJ.Doc] -> HPJ.Doc
hpjEncloseSep open close sep list = open HPJ.<> HPJ.sep (HPJ.punctuate sep list) HPJ.<> close

hpjJSON :: Value -> HPJ.Doc
hpjJSON (Bool True) = HPJ.text "true"
hpjJSON (Bool False) = HPJ.text "false"
hpjJSON (Object o) = hpjEncloseSep (HPJ.text "{") (HPJ.text "}") (HPJ.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v) = HPJ.text (show k) HPJ.<> HPJ.text ":" HPJ.<+> hpjJSON v
hpjJSON (String s) = HPJ.text (show s)
hpjJSON (Array a) =  hpjEncloseSep (HPJ.text "[") (HPJ.text "]") (HPJ.text ",") (map hpjJSON $ toList a)
hpjJSON Null = HPJ.empty
hpjJSON (Number n) = HPJ.text (show n)

pcTest, wlTest, hpjTest :: Value -> String
pcTest = PC.render . prettiestJSON
wlTest inpJson = WL.displayS (WL.renderPretty 1 80 (wlJSON inpJson)) ""
hpjTest = HPJ.render . hpjJSON

readJSONValue fname = do
  inptxt <- BS.readFile fname
  let Right inpJson = parseOnly json' inptxt
  return inpJson

testLibs :: IO ()
testLibs = do
  inpJson <- readJSONValue "small.json"
  putStrLn $ PC.render $ prettiestJSON $ inpJson
  putStrLn $ WL.displayS (WL.renderPretty 1 80 (wlJSON inpJson)) ""
  putStrLn $ HPJ.render $ hpjJSON $ inpJson

{-> testLibs

[{"email": "parsonsgutierrez@jumpstack.com",
  "phone": "+1 (939) 549-3574",
  "_id": "58f859497aedc03cdeca7c49",
  "latitude": 87.026801,
  "favoriteFruit": "apple",
  "age": 34.0,
  "balance": "$3,560.54",
  "address": "303 Brigham Street, Boyd, Idaho, 301",
  "greeting": "Hello, Parsons Gutierrez! You have 10 unread messages.",
  "eyeColor": "brown",
  "picture": "http://placehold.it/32x32",
  "gender": "male",
  "name": "Parsons Gutierrez",
  "guid": "6b93a496-e422-41a2-abb9-86627a7804f6",
  "company": "JUMPSTACK",
  "registered": "2014-05-21T04:36:34 -02:00",
  "longitude": -114.952111,
  "index": 0.0,
  "isActive": false,
  "tags": ["reprehenderit",
           "excepteur",
           "consequat",
           "est",
           "anim",
           "dolor",
           "commodo"],
  "friends": [{"name": "Sabrina Banks", "id": 0.0},
              {"name": "Tanya Walters", "id": 1.0},
              {"name": "Heath Gould", "id": 2.0}]}]
-}

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal --sandbox-config-file=../cabal.sandbox.config repl --only bench")
-- End:
