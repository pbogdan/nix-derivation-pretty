{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nix.Derivation.Pretty.Orphans where

import Data.Text.Prettyprint.Doc

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (x1, x2, x3, x4) = tupled [pretty x1, pretty x2, pretty x3, pretty x4]
