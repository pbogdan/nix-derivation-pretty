{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
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

prettyPath :: FilePath -> Text
prettyPath = Text.drop (Text.length "Filepath ") . show

prettyText :: Text -> Text
prettyText = show

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
prettyPlatform = pretty . prettyText

prettyBuilder :: Text -> Doc ann
prettyBuilder = pretty . prettyText

prettyArgs :: Vec.Vector Text -> Doc ann
prettyArgs = prettyList . map prettyText . Vec.toList

prettyEnv :: Map Text Text -> Doc ann
prettyEnv = pretty . map (prettyText *** prettyText) . Map.toList

prettyDerivation :: Derivation -> Doc ann
prettyDerivation drv =
  "Derive" <+>
  tupled
    [ prettyOutputs (outputs drv)
    , prettyInputDrvs (inputDrvs drv)
    , prettyInputSrcs (inputSrcs drv)
    , prettyPlatform (platform drv)
    , prettyBuilder (builder drv)
    , prettyArgs (args drv)
    , prettyEnv (env drv)
    ]

prettyDerivationHs :: Derivation -> String
prettyDerivationHs = ppShow

#if MIN_VERSION_prettyprinter_ansi_terminal(1,1,0)
prettyDerivationHuman :: Text -> Derivation -> Doc AnsiStyle
#else
prettyDerivationHuman :: Text -> Derivation -> Doc AnsiTerminal
#endif
prettyDerivationHuman p drv =
  ann bold (pretty (p <> ":")) <+>
  line <+>
  line <+>
  align
    (ann bold " outputs:" <+>
     line <+>
     humanOutputs drv <+>
     line <+>
     ann bold "input sources:" <+>
     line <+>
     humanList (map (pretty . prettyPath) . Set.toList . inputSrcs $ drv) <+>
     line <+>
     ann bold "builder:" <+>
     (pretty . builder $ drv) <+>
     line <+>
     ann bold "builder args: " <+>
     line <+>
     (humanList . map pretty . Vec.toList . args $ drv) <+>
     line <+>
     ann bold "environment:" <+>
     line <+>
     (humanList .
      map (\(k, v) -> pretty (k <> ":") <+> line <+> (indent 3 . pretty $ v)) .
      Map.toList . env $
      drv))
  where
#if MIN_VERSION_prettyprinter_ansi_terminal(1,1,0)
    ann = annotate
#else
    ann = identity
#endif


humanList :: [Doc ann] -> Doc ann
humanList = align . sep . map (" " <+>)

humanOutputs :: Derivation -> Doc ann
humanOutputs drv =
  humanList .
  concatMap
    (\(name, out) ->
       [ pretty ("name: " <> name)
       , humanList
           [ pretty ("hash: " <> hash out)
           , pretty ("hash algo: " <> hashAlgo out)
           , pretty ("input derivations: " :: Text)
           , humanList . map (pretty . prettyPath) . Set.toList $
             inputDrvsForOutput drv name
           ]
       ]) .
  Map.toList . outputs $
  drv

inputDrvsForOutput :: Derivation -> Text -> Set FilePath
inputDrvsForOutput drv out =
  let ins = inputDrvs drv
  in Set.fromList . Map.keys . Map.filter (Set.member out) $ ins
