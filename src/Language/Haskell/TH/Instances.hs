{- | A grab bag of useful instances for Template Haskell types -}
module Language.Haskell.TH.Instances where
    
import Language.Haskell.TH
import Data.String
import Test.QuickCheck
import Control.Applicative
import Test.QuickCheck.Instances.Char

instance IsString Name where
    fromString x = mkName x
    
instance Arbitrary Name where
    arbitrary = mkName <$> arbitrary
        
arb_constructor_name = do 
    x <- upperAlpha
    xs <- listOf (oneof [numeric, lowerAlpha, upperAlpha])
    return $ mkName $ x:xs

instance Arbitrary Type where
    arbitrary = sized type_arb

type_arb :: Int -> Gen Type       
type_arb depth = do
        let max_option = if depth > 0 then 8 else (5 :: Int)
        typ <- choose (0, max_option)
        case typ of
            0 -> VarT <$> mkName <$> listOf lowerAlpha
            1 -> ConT <$> arb_constructor_name
            2 -> TupleT <$> arbitrary
            3 -> UnboxedTupleT <$> arbitrary 
            4 -> return ArrowT
            5 -> return ListT
            6 -> forallt_arb (depth - 1)
            7 -> SigT <$> type_arb (depth - 1) <*> arbitrary
            8 -> AppT <$> type_arb (depth - 1) <*> type_arb (depth - 1)
            
--this needs work but I have no use for it now
forallt_arb :: Int -> Gen Type
forallt_arb depth = do
    ForallT <$> (return []) <*> (return []) <*> (return $ VarT $ mkName "test")
    
instance Arbitrary Kind where
    arbitrary = sized kind_arb

kind_arb :: Int -> Gen Kind        
kind_arb depth = do
    let max_option = if depth > 0 then 1 else (2 :: Int)
    typ <- choose(0, max_option)
    case typ of 
        0 -> return StarK
        1 -> ArrowK <$> kind_arb (depth - 1) <*> kind_arb (depth - 1)
    
haskell_98_type_arb depth = do
    let max_option = if depth > 0 then 6 else (5 :: Int)
    typ <- choose (0, max_option)
    case typ of
        0 -> VarT <$> mkName <$> listOf lowerAlpha
        1 -> ConT <$> arb_constructor_name
        2 -> TupleT <$> arbitrary
        3 -> UnboxedTupleT <$> arbitrary 
        4 -> return ArrowT
        5 -> return ListT
        6 -> AppT <$> haskell_98_type_arb (depth - 1) <*> haskell_98_type_arb (depth - 1)
        
        

        
        

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
            