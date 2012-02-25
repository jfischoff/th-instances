{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, EmptyDataDecls #-}
module Main where
    
import Language.Haskell.TH.Instances
import Language.Haskell.TH
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Test.QuickCheck.Checkers
import Debug.Trace
import THFunctions

main = defaultMainWithArgs tests ["-a 15", "-o 4"]

tests = [
        testGroup "Test FromString" [
            testCase "test_from_string" test_from_string
        ]]
        
test_from_string = "Hey" @?= mkName "Hey"



--test that the gen data decs type check
$( runIO $ do
              d <- data_gen' 100
              return d
 )
