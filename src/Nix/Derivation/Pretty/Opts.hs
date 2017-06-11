{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Derivation.Pretty.Opts
  ( Style(..)
  , WrapWidth(..)
  , Options(..)
  , parseOptions
  ) where

import Protolude

import GHC.Show
import Options.Applicative

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
parseWrapWidth = eitherReader $ \s -> case readEither s of
  Left err -> Left err
  Right x -> Right . WrapWidth $ x

data Options =
  Options Style
          WrapWidth
          Text
  deriving (Eq, Show)

style :: Parser Style
style =
  flag'
    Haskell
    (long "haskell" <>
     help "Pretty print haskell data type representing the derivation") <|>
  flag' Human (long "human" <> help "Pretty print more human friendly output") <|>
  pure Pretty

width :: Parser WrapWidth
width =
  option
    parseWrapWidth
    (short 'w' <> long "width" <> help "Column to wrap the output at" <>
     showDefault <>
     value 80 <>
     metavar "int")

options :: Parser Options
options =
  Options <$> style <*> width <*> (toS <$> argument str (metavar "file"))

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

parseOptions :: IO Options
parseOptions = execParser (options `withInfo` "Pretty print a Nix derivation.")
