{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, FlexibleContexts, RankNTypes, TypeFamilies, LambdaCase #-}

module BenchmarkXML where

import qualified Text.PrettyPrint.Compact as PC
import qualified Text.PrettyPrint.Leijen as WL
import qualified Text.PrettyPrint.HughesPJ as HPJ
import Data.Monoid
import Text.XML.Light.Input
import Text.XML.Light.Types

type DOC = PC.Doc ()

pcXML :: Element -> DOC
pcXML Element{..} =
  PC.encloseSep
    (PC.encloseSep (PC.text "<" <> PC.text (qName elName)) (PC.text ">")
      (PC.text " ") (map pcAttrib (elAttribs)))
    (PC.text "</" <> PC.text (qName elName) <> PC.text ">")
    mempty
    (map pcContent elContent)

pcAttrib :: Attr -> DOC
pcAttrib Attr{..} = PC.text (qName attrKey) <> PC.text "=" <> PC.text (show attrVal)

pcContent :: Content -> DOC
pcContent (Elem e) = pcXML e
pcContent (Text CData{..}) = PC.text cdData
pcContent (CRef r) = PC.text r

wlXML :: Element -> WL.Doc
wlXML Element{..} =
  WL.encloseSep
    (WL.encloseSep (WL.text "<" WL.<> WL.text (qName elName)) (WL.text ">")
      (WL.text " ") (map wlAttrib (elAttribs)))
    (WL.text "</" WL.<> WL.text (qName elName) WL.<> WL.text ">")
    WL.empty
    (map wlContent elContent)

wlAttrib :: Attr -> WL.Doc
wlAttrib Attr{..} = WL.text (qName attrKey) WL.<> WL.text "=" WL.<> WL.text (show attrVal)

wlContent :: Content -> WL.Doc
wlContent (Elem e) = wlXML e
wlContent (Text CData{..}) = WL.text cdData
wlContent (CRef r) = WL.text r

hpjEncloseSep :: HPJ.Doc -> HPJ.Doc -> HPJ.Doc -> [HPJ.Doc] -> HPJ.Doc
hpjEncloseSep open close sep list = open HPJ.<> HPJ.sep (HPJ.punctuate sep list) HPJ.<> close

hpjXML :: Element -> HPJ.Doc
hpjXML Element{..} =
  hpjEncloseSep
    (hpjEncloseSep (HPJ.text "<" HPJ.<> HPJ.text (qName elName)) (HPJ.text ">")
      (HPJ.text " ") (map hpjAttrib (elAttribs)))
    (HPJ.text "</" HPJ.<> HPJ.text (qName elName) HPJ.<> HPJ.text ">")
    HPJ.empty
    (map hpjContent elContent)

hpjAttrib :: Attr -> HPJ.Doc
hpjAttrib Attr{..} = HPJ.text (qName attrKey) HPJ.<> HPJ.text "=" HPJ.<> HPJ.text (show attrVal)

hpjContent :: Content -> HPJ.Doc
hpjContent (Elem e) = hpjXML e
hpjContent (Text CData{..}) = HPJ.text cdData
hpjContent (CRef r) = HPJ.text r

pcTest, wlTest, hpjTest :: Element -> String
pcTest = PC.render . pcXML
wlTest inpXml = WL.displayS (WL.renderPretty 1 80 (wlXML inpXml)) ""
hpjTest = HPJ.render . hpjXML

readXMLValue :: String -> IO Element
readXMLValue fname = do
  inptxt <- readFile fname
  case parseXMLDoc inptxt of
    Just x -> return x
    Nothing -> error "XML Parse error"

testLibs :: IO ()
testLibs = do
  inpXml <- readXMLValue "benchdata/cds.xml"
  putStrLn $ pcTest inpXml
  putStrLn $ WL.displayS (WL.renderPretty 1 80 (wlXML inpXml)) ""
  putStrLn $ HPJ.render $ hpjXML $ inpXml

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
