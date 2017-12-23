{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Derivation.Pretty.Opts
  ( Style(..)
  , WrapWidth(..)
  , Options(..)
  , parseOptions
  ) where

import           Protolude

import qualified Data.Text as Text
import           GHC.Show
import           Options.Applicative (Parser, ParserInfo, ReadM)
import qualified Options.Applicative as Options

data Style
  = Pretty
  | Haskell
  | Human
  deriving (Eq, Show)

newtype WrapWidth =
  WrapWidth Int
  deriving (Eq, Read, Num)

instance GHC.Show.Show WrapWidth where
  show (WrapWidth x) = GHC.Show.show x

parseWrapWidth :: ReadM WrapWidth
parseWrapWidth = Options.eitherReader $ \s -> case readEither s of
  Left err -> Left err
  Right x -> Right . WrapWidth $ x

data Options =
  Options Style
          WrapWidth
          Text
  deriving (Eq, Show)

style :: Parser Style
style =
  Options.flag'
    Haskell
    (Options.long "haskell" <>
     Options.help "Pretty print haskell data type representing the derivation") <|>
  Options.flag'
    Human
    (Options.long "human" <>
     Options.help "Pretty print more human friendly output") <|>
  pure Pretty

width :: Parser WrapWidth
width =
  Options.option
    parseWrapWidth
    (Options.short 'w' <> Options.long "width" <>
     Options.help "Column to wrap the output at" <>
     Options.showDefault <>
     Options.value 80 <>
     Options.metavar "int")

options :: Parser Options
options =
  Options <$> style <*> width <*>
  (Text.pack <$> Options.argument Options.str (Options.metavar "file"))

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc =
  Options.info (Options.helper <*> opts) $ Options.progDesc (toS desc)

parseOptions :: IO Options
parseOptions =
  Options.execParser (options `withInfo` "Pretty print a Nix derivation.")
