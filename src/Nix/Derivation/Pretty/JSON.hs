{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nix.Derivation.Pretty.JSON where

import           Protolude hiding (FilePath, hash)

import           Data.Aeson
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector
import           Filesystem.Path
import           Nix.Derivation (Derivation, DerivationOutput)
import qualified Nix.Derivation as Drv

unquote :: Text -> Text
unquote = Text.tail . Text.init

jsonPath :: FilePath -> Text
jsonPath = unquote . Text.drop (Text.length "Filepath ") . show

data JSONDerivation = JSONDerivation
  { outputs :: Map Text JSONDerivationOutput
  , inputDrvs :: Map Text (Set Text)
  , inputSrcs :: Set Text
  , platform :: Text
  , builder :: Text
  , args :: Vector Text
  , env :: Map Text Text
  } deriving (Generic)

data JSONDerivationOutput = JSONDerivationOutput
  { path :: Text
  , hashAlgo :: Text
  , hash :: Text
  } deriving (Generic)

toJSONDerivation :: Derivation -> JSONDerivation
toJSONDerivation drv =
  JSONDerivation
    { outputs = Map.map toJSONDerivationOutput . Drv.outputs $ drv
    , inputDrvs = Map.mapKeys jsonPath . Drv.inputDrvs $ drv
    , inputSrcs = Set.map jsonPath . Drv.inputSrcs $ drv
    , platform = Drv.platform drv
    , builder = Drv.builder drv
    , args = Drv.args drv
    , env = Drv.env drv
    }

toJSONDerivationOutput :: DerivationOutput -> JSONDerivationOutput
toJSONDerivationOutput out =
  JSONDerivationOutput
    { path = jsonPath . Drv.path $ out
    , hashAlgo = Drv.hashAlgo out
    , hash = Drv.hash out
    }

instance ToJSON JSONDerivation where
instance ToJSON JSONDerivationOutput where
