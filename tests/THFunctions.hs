{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module THFunctions where
import Test.QuickCheck.Checkers
import Language.Haskell.TH.Instances
import Language.Haskell.TH
import Control.Monad.Reader hiding (lift)

data_gen = head `fmap` gens 1 (haskell_98_data_dec primitive_types)

data_gen' x = gens x (haskell_98_data_dec (primitive_types ++ [$(lift =<<  explictly_kind ''ReaderT)]))