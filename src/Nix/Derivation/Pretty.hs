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
import           Data.Bitraversable
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LText
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Vector as Vec
import           Extra (systemOutput)
import           Filesystem.Path (FilePath)
import           Nix.Derivation
import           Nix.Derivation.Pretty.Opts
import           Nix.Derivation.Pretty.Orphans ()
import           Shell.Command
import           System.Exit (ExitCode(..))
import           System.IO.Temp
import           Text.Show.Pretty (ppShow)


defaultMain :: IO ()
defaultMain = do
  Options s (WrapWidth w) drvs@(Drvs p1 p2) <- parseOptions
  drvsOrErr <-
    bisequence <$> (bisequence . bimap (ppDrv s w) (ppDrv s w) $ drvs)
  case drvsOrErr of
    Left e -> do
      putText $ "Pretty printing failed: " <> e
      exitFailure
    Right (Drvs x Nothing) -> putText x
    Right (Drvs x (Just y)) -> do
      diffOrErr <- diffDrvs x y
      case diffOrErr of
        Left e -> do
          putText $ "Diffing failed: " <> e
          exitFailure
        Right diff ->
          putText .
          Text.replace "@t1@" (toS p1) .
          Text.replace "@t2@" (toS . fromMaybe "" $ p2) $
          diff

ppDrv :: Style -> Int -> Protolude.FilePath -> IO (Either Text Text)
ppDrv s _w p = do
  text <- LText.readFile p
  let result = parse parseDerivation text
  case result of
    Done _ r -> do
      let doc = prettyDerivation r
      case s of
        Pretty -> return . Right . show $ doc
        Haskell -> return . Right . toS . prettyDerivationHs $ r
        Human -> return . Right . show . prettyDerivationHuman (toS p) $ r
    Fail _ _ err -> return . Left . toS $ err

diffDrvs :: Text -> Text -> IO (Either Text Text)
diffDrvs drv1 drv2 =
  withSystemTempFile "pp-drv-diff" $ \t1 h1 ->
    withSystemTempFile "pp-drv-diff" $ \t2 h2 -> do
      liftIO . Text.hPutStr h1 $ drv1
      liftIO . Text.hPutStr h2 $ drv2
      let cmd =
            command "diff" $ do
              switch "-u5"
              arg . toS $ t1
              arg . toS $ t2
      ret <- liftIO . systemOutput . toS $ cmd
      -- `man diff` for meaning of the exit codes
      case ret of
        (ExitSuccess, _) -> return . Right $ ""
        (ExitFailure 1, diff) ->
          return .
          Right .
          Text.replace (toS t1) "@t1@" . Text.replace (toS t2) "@t2@" . toS $
          diff
        (ExitFailure _, out) ->
          return . Left $ "Generating diff failed: " <> toS out

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
  Map.toAscList


prettyInputDrvs :: Map FilePath (Set Text) -> Doc ann
prettyInputDrvs =
  prettyList .
  map (prettyPath *** map prettyText . Set.toAscList) . Map.toAscList

prettyInputSrcs :: Set FilePath -> Doc ann
prettyInputSrcs = prettyList . map prettyPath . Set.toAscList

prettyPlatform :: Text -> Doc ann
prettyPlatform = pretty . prettyText

prettyBuilder :: Text -> Doc ann
prettyBuilder = pretty . prettyText

prettyArgs :: Vec.Vector Text -> Doc ann
prettyArgs = prettyList . map prettyText . sort . Vec.toList

prettyEnv :: Map Text Text -> Doc ann
prettyEnv = pretty . map (prettyText *** prettyText) . Map.toAscList

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
     humanList (map (pretty . prettyPath) . Set.toAscList . inputSrcs $ drv) <+>
     line <+>
     bold "builder:" <+>
     (pretty . builder $ drv) <+>
     line <+>
     bold "builder args: " <+>
     line <+>
     (humanList . map pretty . sort . Vec.toList . args $ drv) <+>
     line <+>
     bold "environment:" <+>
     line <+>
     (humanList .
      map (\(k, v) -> pretty (k <> ":") <+> line <+> (indent 3 . pretty $ v)) .
      Map.toAscList . env $
      drv))

humanList :: [Doc ann] -> Doc ann
humanList = align . sep . map (" " <+>)

humanOutputs :: Derivation -> Doc AnsiTerminal
humanOutputs drv =
  humanList .
  concatMap
    (\(name, out) ->
       [ pretty ("name: " <> name)
       , humanList
           [ pretty ("hash: " <> hash out)
           , pretty ("hash algo: " <> hashAlgo out)
           , pretty ("input derivations: " :: Text)
           , humanList . map (pretty . prettyPath) . Set.toAscList $
             inputDrvsForOutput drv name
           ]
       ]) .
  Map.toAscList . outputs $
  drv

inputDrvsForOutput :: Derivation -> Text -> Set FilePath
inputDrvsForOutput drv out =
  let ins = inputDrvs drv
  in Set.fromList . Map.keys . Map.filter (Set.member out) $ ins
