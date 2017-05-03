{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

module BenchmarkXML where

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

import qualified Text.PrettyPrint.Compact as PC
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import Data.Foldable (toList)
import qualified Text.PrettyPrint.Leijen as WL
import qualified Text.PrettyPrint.HughesPJ as HPJ
import Data.Monoid
import Text.XML.Light.Input
import Text.XML.Light.Types

pcXML :: Element -> PC.Doc
pcXML Element{..} =
  PC.encloseSep
    (PC.encloseSep (PC.text "<" <> PC.text (qName elName)) (PC.text ">")
      (PC.text " ") (map pcAttrib (elAttribs)))
    mempty
    (PC.text "</" <> PC.text (qName elName) <> PC.text ">")
    (map pcContent elContent)

pcAttrib :: Attr -> PC.Doc
pcAttrib Attr{..} = PC.text (qName attrKey) <> PC.text "=" <> PC.text (show attrVal)

pcContent :: Content -> PC.Doc
pcContent (Elem e) = pcXML e
pcContent (Text CData{..}) = PC.text cdData
pcContent (CRef r) = PC.text r

-- prettiestXML :: Value -> PC.Doc
-- prettiestXML (Bool True) = PC.text "true"
-- prettiestXML (Bool False) = PC.text "false"
-- prettiestXML (Object o) = PC.encloseSep (PC.text "{") (PC.text "}") (PC.text ",") (map prettyKV $ H.toList o)
--   where prettyKV (k,v) = PC.text (show k) PC.<> PC.text ":" PC.<+> prettiestXML v
-- prettiestXML (String s) = PC.string (show s)
-- prettiestXML (Array a) =  PC.encloseSep (PC.text "[") (PC.text "]") (PC.text ",") (map prettiestXML $ toList a)
-- prettiestXML Null = PC.mempty
-- prettiestXML (Number n) = PC.text (show n)

-- wlXML :: Value -> WL.Doc
-- wlXML (Bool True) = WL.text "true"
-- wlXML (Bool False) = WL.text "false"
-- wlXML (Object o) = WL.encloseSep (WL.text "{") (WL.text "}") (WL.text ",") (map prettyKV $ H.toList o)
--   where prettyKV (k,v) = WL.text (show k) WL.<> WL.text ":" WL.<+> wlXML v
-- wlXML (String s) = WL.string (show s)
-- wlXML (Array a) =  WL.encloseSep (WL.text "[") (WL.text "]") (WL.text ",") (map wlXML $ toList a)
-- wlXML Null = WL.empty
-- wlXML (Number n) = WL.text (show n)

-- hpjEncloseSep :: HPJ.Doc -> HPJ.Doc -> HPJ.Doc -> [HPJ.Doc] -> HPJ.Doc
-- hpjEncloseSep open close sep list = open HPJ.<> HPJ.sep (HPJ.punctuate sep list) HPJ.<> close

-- hpjXML :: Value -> HPJ.Doc
-- hpjXML (Bool True) = HPJ.text "true"
-- hpjXML (Bool False) = HPJ.text "false"
-- hpjXML (Object o) = hpjEncloseSep (HPJ.text "{") (HPJ.text "}") (HPJ.text ",") (map prettyKV $ H.toList o)
--   where prettyKV (k,v) = HPJ.text (show k) HPJ.<> HPJ.text ":" HPJ.<+> hpjXML v
-- hpjXML (String s) = HPJ.text (show s)
-- hpjXML (Array a) =  hpjEncloseSep (HPJ.text "[") (HPJ.text "]") (HPJ.text ",") (map hpjXML $ toList a)
-- hpjXML Null = HPJ.empty
-- hpjXML (Number n) = HPJ.text (show n)

pcTest{-, wlTest, hpjTest-} :: Element -> String
pcTest = PC.render . pcXML
-- wlTest inpXml = WL.displayS (WL.renderPretty 1 80 (wlXML inpXml)) ""
-- hpjTest = HPJ.render . hpjXML

readXMLValue fname = do
  inptxt <- readFile fname
  return $ case parseXMLDoc inptxt of
    Just x -> return x
    Nothing -> error "XML Parse error"

testLibs :: IO ()
testLibs = do
  Right inpXml <- readXMLValue "benchdata/cds.xml"
  putStrLn $ pcTest inpXml
  -- putStrLn $ WL.displayS (WL.renderPretty 1 80 (wlXML inpXml)) ""
  -- putStrLn $ HPJ.render $ hpjXML $ inpXml

{-> testLibs

<CATALOG>
 <COLLECTION>
           <CD>
           <TITLE>Empire Burlesque</TITLE>
           
           <ARTIST>Bob Dylan</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>Columbia</COMPANY>
           
           <PRICE>10.90</PRICE>
           
           <YEAR>1985</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Hide your heart</TITLE>
           
           <ARTIST>Bonnie Tyler</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>CBS Records</COMPANY>
           
           <PRICE>9.90</PRICE>
           
           <YEAR>1988</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Greatest Hits</TITLE>
           
           <ARTIST>Dolly Parton</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>RCA</COMPANY>
           
           <PRICE>9.90</PRICE>
           
           <YEAR>1982</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Still got the blues</TITLE>
           
           <ARTIST>Gary Moore</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Virgin records</COMPANY>
           
           <PRICE>10.20</PRICE>
           
           <YEAR>1990</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Eros</TITLE>
           
           <ARTIST>Eros Ramazzotti</ARTIST>
           
           <COUNTRY>EU</COUNTRY>
           
           <COMPANY>BMG</COMPANY>
           
           <PRICE>9.90</PRICE>
           
           <YEAR>1997</YEAR>
           
</CD>
           
           <CD>
           <TITLE>One night only</TITLE>
           
           <ARTIST>Bee Gees</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Polydor</COMPANY>
           
           <PRICE>10.90</PRICE>
           
           <YEAR>1998</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Sylvias Mother</TITLE>
           
           <ARTIST>Dr.Hook</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>CBS</COMPANY>
           
           <PRICE>8.10</PRICE>
           
           <YEAR>1973</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Maggie May</TITLE>
           
           <ARTIST>Rod Stewart</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Pickwick</COMPANY>
           
           <PRICE>8.50</PRICE>
           
           <YEAR>1990</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Romanza</TITLE>
           
           <ARTIST>Andrea Bocelli</ARTIST>
           
           <COUNTRY>EU</COUNTRY>
           
           <COMPANY>Polydor</COMPANY>
           
           <PRICE>10.80</PRICE>
           
           <YEAR>1996</YEAR>
           
</CD>
           
           <CD>
           <TITLE>When a man loves a woman</TITLE>
           
           <ARTIST>Percy Sledge</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>Atlantic</COMPANY>
           
           <PRICE>8.70</PRICE>
           
           <YEAR>1987</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Black angel</TITLE>
           
           <ARTIST>Savage Rose</ARTIST>
           
           <COUNTRY>EU</COUNTRY>
           
           <COMPANY>Mega</COMPANY>
           
           <PRICE>10.90</PRICE>
           
           <YEAR>1995</YEAR>
           
</CD>
           
           <CD>
           <TITLE>1999 Grammy Nominees</TITLE>
           
           <ARTIST>Many</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>Grammy</COMPANY>
           
           <PRICE>10.20</PRICE>
           
           <YEAR>1999</YEAR>
           
</CD>
           
           <CD>
           <TITLE>For the good times</TITLE>
           
           <ARTIST>Kenny Rogers</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Mucik Master</COMPANY>
           
           <PRICE>8.70</PRICE>
           
           <YEAR>1995</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Big Willie style</TITLE>
           
           <ARTIST>Will Smith</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>Columbia</COMPANY>
           
           <PRICE>9.90</PRICE>
           
           <YEAR>1997</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Tupelo Honey</TITLE>
           
           <ARTIST>Van Morrison</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Polydor</COMPANY>
           
           <PRICE>8.20</PRICE>
           
           <YEAR>1971</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Soulsville</TITLE>
           
           <ARTIST>Jorn Hoel</ARTIST>
           
           <COUNTRY>Norway</COUNTRY>
           
           <COMPANY>WEA</COMPANY>
           
           <PRICE>7.90</PRICE>
           
           <YEAR>1996</YEAR>
           
</CD>
           
           <CD>
           <TITLE>The very best of</TITLE>
           
           <ARTIST>Cat Stevens</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Island</COMPANY>
           
           <PRICE>8.90</PRICE>
           
           <YEAR>1990</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Stop</TITLE>
           
           <ARTIST>Sam Brown</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>A and M</COMPANY>
           
           <PRICE>8.90</PRICE>
           
           <YEAR>1988</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Bridge of Spies</TITLE>
           
           <ARTIST>T'Pau</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Siren</COMPANY>
           
           <PRICE>7.90</PRICE>
           
           <YEAR>1987</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Private Dancer</TITLE>
           
           <ARTIST>Tina Turner</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>Capitol</COMPANY>
           
           <PRICE>8.90</PRICE>
           
           <YEAR>1983</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Midt om natten</TITLE>
           
           <ARTIST>Kim Larsen</ARTIST>
           
           <COUNTRY>EU</COUNTRY>
           
           <COMPANY>Medley</COMPANY>
           
           <PRICE>7.80</PRICE>
           
           <YEAR>1983</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Pavarotti Gala Concert</TITLE>
           
           <ARTIST>Luciano Pavarotti</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>DECCA</COMPANY>
           
           <PRICE>9.90</PRICE>
           
           <YEAR>1991</YEAR>
           
</CD>
           
           <CD>
           <TITLE>The dock of the bay</TITLE>
           
           <ARTIST>Otis Redding</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>Stax Records</COMPANY>
           
           <PRICE>7.90</PRICE>
           
           <YEAR>1968</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Picture book</TITLE>
           
           <ARTIST>Simply Red</ARTIST>
           
           <COUNTRY>EU</COUNTRY>
           
           <COMPANY>Elektra</COMPANY>
           
           <PRICE>7.20</PRICE>
           
           <YEAR>1985</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Red</TITLE>
           
           <ARTIST>The Communards</ARTIST>
           
           <COUNTRY>UK</COUNTRY>
           
           <COMPANY>London</COMPANY>
           
           <PRICE>7.80</PRICE>
           
           <YEAR>1987</YEAR>
           
</CD>
           
           <CD>
           <TITLE>Unchain my heart</TITLE>
           
           <ARTIST>Joe Cocker</ARTIST>
           
           <COUNTRY>USA</COUNTRY>
           
           <COMPANY>EMI</COMPANY>
           
           <PRICE>8.20</PRICE>
           
           <YEAR>1987</YEAR>
           
</CD>
           
</COLLECTION> 
</CATALOG>
-}

-- Local Variables:
-- dante-project-root: "~/repo/prettiest/paper"
-- dante-target: "bench"
-- dante-repl-command-line: ("nix-shell" "../.styx/shell.nix" "--run" "cabal --sandbox-config-file=../cabal.sandbox.config repl --only bench")
-- End:
