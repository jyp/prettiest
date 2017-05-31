module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson (Value (..), decode)
import Data.Foldable (toList)

import qualified Criterion.Main as C
import qualified Data.HashMap.Lazy as H
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import qualified Text.PrettyPrint.Compact  as PC
import qualified Text.PrettyPrint.HughesPJ as HPJ
import qualified Text.PrettyPrint.Leijen   as WL

prettiestJSON :: Value -> PC.Doc ()
prettiestJSON (Bool True)  = PC.text "true"
prettiestJSON (Bool False) = PC.text "false"
prettiestJSON (Object o)   = PC.encloseSep (PC.text "{") (PC.text "}") (PC.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v)     = PC.text (show k) PC.<> PC.text ":" PC.<+> prettiestJSON v
prettiestJSON (String s)   = PC.string (show s)
prettiestJSON (Array a)    = PC.encloseSep (PC.text "[") (PC.text "]") (PC.text ",") (map prettiestJSON $ toList a)
prettiestJSON Null         = PC.mempty
prettiestJSON (Number n)   = PC.text (show n)

pcRenderText :: PC.Doc a -> TL.Text
pcRenderText = TLB.toLazyText . PC.renderWith (\_ -> TLB.fromString)

wlJSON :: Value -> WL.Doc
wlJSON (Bool True)     = WL.text "true"
wlJSON (Bool False)    = WL.text "false"
wlJSON (Object o)      = WL.encloseSep (WL.text "{") (WL.text "}") (WL.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v) = WL.text (show k) WL.<> WL.text ":" WL.<+> wlJSON v
wlJSON (String s)      = WL.string (show s)
wlJSON (Array a)       = WL.encloseSep (WL.text "[") (WL.text "]") (WL.text ",") (map wlJSON $ toList a)
wlJSON Null            = WL.empty
wlJSON (Number n)      = WL.text (show n)

wlRender :: WL.Doc -> String
wlRender d = WL.displayS (WL.renderPretty 1 80 d) ""

hpjEncloseSep :: HPJ.Doc -> HPJ.Doc -> HPJ.Doc -> [HPJ.Doc] -> HPJ.Doc
hpjEncloseSep open close sep list = open HPJ.<> HPJ.sep (HPJ.punctuate sep list) HPJ.<> close

hpjJSON :: Value -> HPJ.Doc
hpjJSON (Bool True)    = HPJ.text "true"
hpjJSON (Bool False)   = HPJ.text "false"
hpjJSON (Object o)     = hpjEncloseSep (HPJ.text "{") (HPJ.text "}") (HPJ.text ",") (map prettyKV $ H.toList o)
  where prettyKV (k,v) = HPJ.text (show k) HPJ.<> HPJ.text ":" HPJ.<+> hpjJSON v
hpjJSON (String s)     = HPJ.text (show s)
hpjJSON (Array a)      = hpjEncloseSep (HPJ.text "[") (HPJ.text "]") (HPJ.text ",") (map hpjJSON $ toList a)
hpjJSON Null           = HPJ.empty
hpjJSON (Number n)     = HPJ.text (show n)

readJSONValue :: FilePath -> IO Value
readJSONValue fname = do
    contents <- BSL.readFile fname
    let Just inpJson = decode contents
    return inpJson

main :: IO ()
main = do
    smallValue <- evaluate . force =<< readJSONValue "bench/small.json"
    bigValue <- evaluate . force =<< readJSONValue "bench/big.json"

    C.defaultMain
        [ C.bgroup "small"
            [ C.bench "pretty-compact" $ C.nf (PC.render . prettiestJSON) smallValue
            , C.bench "pretty-compact Text" $ C.nf (pcRenderText . prettiestJSON) smallValue
            , C.bench "pretty"         $ C.nf (HPJ.render . hpjJSON) smallValue
            , C.bench "wl-pprint"      $ C.nf (wlRender . wlJSON) smallValue
            ]
        , C.bgroup "big"
            [ C.bench "pretty-compact" $ C.nf (PC.render . prettiestJSON) bigValue
            , C.bench "pretty-compact Text" $ C.nf (pcRenderText . prettiestJSON) bigValue
            , C.bench "pretty"         $ C.nf (HPJ.render . hpjJSON) bigValue 
            , C.bench "wl-pprint"      $ C.nf (wlRender . wlJSON) bigValue
            ]
        ]
