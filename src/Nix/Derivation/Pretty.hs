{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Derivation.Pretty
  ( defaultMain
  , ppDrv
  , prettyDerivation
  , prettyDerivationHs
  , prettyDerivationHuman
  ) where

import qualified Protolude (FilePath)
import           Protolude hiding (FilePath)

import           Control.Arrow ((***))
import           Data.Attoparsec.Text.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as LText
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Text.Prettyprint.Doc.Util
import qualified Data.Vector as Vec
import           Filesystem.Path (FilePath)
import           Nix.Derivation
import           Nix.Derivation.Pretty.Opts
import           Nix.Derivation.Pretty.Orphans ()
import           Text.Show.Pretty (ppShow)

defaultMain :: IO ()
defaultMain = do
  Options s (WrapWidth w) p <- parseOptions
  ppDrv (toS p) s w

ppDrv :: Protolude.FilePath -> Style -> Int -> IO ()
ppDrv p s w = do
  text <- LText.readFile p
  let result = parse parseDerivation text
  case result of
    Done _ r -> do
      let doc = prettyDerivation r
      case s of
        Pretty -> putDocW w doc
        Haskell -> putText . toS . prettyDerivationHs $ r
        Human -> putDoc (prettyDerivationHuman (toS p) r)
    Fail _ _ err -> putText $ "Parsing failed: " <> toS err

prettyPath :: FilePath -> Doc ann
prettyPath =  pretty . Text.drop (Text.length "Filepath ") . show

prettyText :: Text -> Doc ann
prettyText t = pretty ("\"" :: Text) <> pretty t <> pretty ("\"" :: Text)

prettyOutputs :: Map Text DerivationOutput -> Doc ann
prettyOutputs =
  prettyList .
  map
    (\(name, out) ->
       ( prettyText name
       , prettyPath . path $ out
       , prettyText . hashAlgo $ out
       , prettyText . hash $out)) .
  Map.toList

prettyInputDrvs :: Map FilePath (Set Text) -> Doc ann
prettyInputDrvs =
  prettyList . map (prettyPath *** map prettyText . Set.toList) . Map.toList

prettyInputSrcs :: Set FilePath -> Doc ann
prettyInputSrcs = prettyList . map prettyPath . Set.toList

prettyPlatform :: Text -> Doc ann
prettyPlatform = prettyText

prettyBuilder :: Text -> Doc ann
prettyBuilder = prettyText

prettyArgs :: Vec.Vector Text -> Doc ann
prettyArgs = prettyList . map prettyText . Vec.toList

prettyEnv :: Map Text Text -> Doc ann
prettyEnv = pretty . map (prettyText *** prettyText) . Map.toList

prettyDerivation :: Derivation -> Doc ann
prettyDerivation drv =
  "Derive" <+>
  (tupled
     ([ prettyOutputs (outputs drv)
      , prettyInputDrvs (inputDrvs drv)
      , prettyInputSrcs (inputSrcs drv)
      , prettyPlatform (platform drv)
      , prettyBuilder (builder drv)
      , prettyArgs (args drv)
      , prettyEnv (env drv)
      ]))

prettyDerivationHs :: Derivation -> String
prettyDerivationHs = ppShow

prettyDerivationHuman :: Text -> Derivation -> Doc AnsiTerminal
prettyDerivationHuman p drv =
  (bold . pretty $ (p <> ":")) <+>
  line <+>
  line <+>
  align
    (bold " outputs:" <+>
     line <+>
     humanOutputs drv <+>
     line <+>
     bold "input sources:" <+>
     line <+>
     humanList (map prettyPath . Set.toList . inputSrcs $ drv) <+>
     line <+>
     bold "builder:" <+>
     (pretty . builder $ drv) <+>
     line <+>
     bold "builder args: " <+>
     line <+>
     (humanList . map pretty . Vec.toList . args $ drv) <+>
     line <+>
     bold "environment:" <+>
     line <+>
     (humanList .
      map (\(k, v) -> pretty (k <> ":") <+> line <+> (indent 3 . pretty $ v)) .
      Map.toList . env $
      drv))

humanList :: [Doc ann] -> Doc ann
humanList = align . sep . zipWith (<+>) (repeat " ")

humanOutputs :: Derivation -> Doc AnsiTerminal
humanOutputs drv =
  humanList .
  concatMap
    (\(name, out) ->
       [ pretty ("name: " <> name)
       , humanList
           [ pretty ("hash: " <> hash out)
           , pretty ("hash algo: " <> hashAlgo out)
           , pretty ("input deriviations: " :: Text)
           , humanList . map prettyPath . Set.toList $
             inputDrvsForOutput drv name
           ]
       ]) .
  Map.toList . outputs $
  drv


inputDrvsForOutput :: Derivation -> Text -> Set FilePath
inputDrvsForOutput drv out =
  let ins = inputDrvs drv
  in Set.fromList . Map.keys . Map.filter (\x -> Set.member out x) $ ins
