{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Derivation.Pretty.Opts
  ( Style(..)
  , Drvs(..)
  , Options(..)
  , parseOptions
  ) where

import Protolude

import Data.Bifoldable
import Data.Bitraversable
import Options.Applicative

data Drvs a b =
  Drvs a
       (Maybe b)
  deriving (Eq, Show)

instance Functor (Drvs a) where
  fmap f (Drvs x y) = Drvs x (f <$> y)

instance Foldable (Drvs a) where
  foldr f z (Drvs _ (Just y)) = f y z
  foldr _ z (Drvs _ Nothing) = z

instance Traversable (Drvs a) where
  traverse f (Drvs x y) = Drvs <$> pure x <*> traverse f y

instance Bifunctor Drvs where
  bimap f g (Drvs x y) = Drvs (f x) (g <$> y)

instance Bifoldable Drvs where
  bifoldMap _ g (Drvs _ (Just y)) = g y
  bifoldMap f _ (Drvs x Nothing) = f x

instance Bitraversable Drvs where
  bitraverse f g (Drvs x y) = Drvs <$> f x <*> traverse g y

data Style
  = Pretty
  | Haskell
  | Human
  deriving (Eq, Show)

data Options =
  Options Style
          (Drvs FilePath FilePath)
  deriving (Eq, Show)

style :: Parser Style
style =
  flag'
    Haskell
    (long "haskell" <>
     help "Pretty print haskell data type representing the derivation") <|>
  flag' Human (long "human" <> help "Pretty print more human friendly output") <|>
  pure Pretty

drvs :: Parser (Drvs FilePath FilePath)
drvs =
  Drvs <$> argument str (metavar "file") <*>
  optional
    (strOption
       (short 'd' <> long "diff-against" <> help "Diff against another .drv" <>
        metavar "file"))

options :: Parser Options
options = Options <$> style <*> drvs

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)

parseOptions :: IO Options
parseOptions = execParser (options `withInfo` "Pretty print a Nix derivation.")
