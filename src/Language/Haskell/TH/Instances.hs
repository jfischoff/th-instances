{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{- | A grab bag of useful instances for Template Haskell types -}
module Language.Haskell.TH.Instances (
    module Language.Haskell.TH.Lift,
    module Language.Haskell.TH.LiftInstances,
    haskell_98_data_dec,
    type_arb, 
    kind_arb,
    haskell_98_type_arb,
    primitive_types
) where
    
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Data.String
import Test.QuickCheck
import Control.Applicative
import Test.QuickCheck.Instances.Char
import Control.Monad.Reader
import Data.List
import Language.Haskell.TH.LiftInstances
import Data.Char

import qualified Language.Haskell.TH.Lift as L

instance IsString Name where
    fromString x = mkName x
    
instance Arbitrary Name where
    arbitrary = mkName <$> arbitrary
        
arb_constructor_name = do 
    x <- upperAlpha
    name_size <- choose (5, 20)
    xs <- sized (\x -> vectorOf name_size (oneof [numeric, lowerAlpha, upperAlpha]))
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
        
letter_strings =  map (\x -> mkName $ (x:[])) $ take 26 ['a'..]
  
--the right way to do this is probably to gen a simply typed environment 
--and sample functions from it, and convert
--what I really need to do, is first I generate 

--name of data -> name of ty_var -> kind
--type KindEnv = [(Name, [(Name, Kind)])]

--look_up_kind env = undefined
--extend name kind = undefined

safe_prefix name =  map (replace_symbol . toLower) name

replace_symbol '[' = 'z'
replace_symbol ']' = 'q'
replace_symbol x = x

collect_kinded_tyvars :: Dec -> [TyVarBndr]
collect_kinded_tyvars (DataD _ _ tyvars _ _ ) = tyvars
collect_kinded_tyvars (NewtypeD _ _ tyvars _ _ ) = tyvars
 
prefix_ty_var :: Name -> TyVarBndr -> TyVarBndr
prefix_ty_var name (KindedTV ty_name kind) = KindedTV (mkName $ (safe_prefix $ nameBase name) ++ ("_") ++ (show ty_name)) kind

prefix_ty_vars :: Dec -> Dec
prefix_ty_vars (DataD x name tyvars z w ) = DataD x name (map (prefix_ty_var name) tyvars) z w
prefix_ty_vars (NewtypeD x name tyvars z w ) = NewtypeD x name (map (prefix_ty_var name) tyvars) z w

ty_var_kind :: TyVarBndr -> Kind
ty_var_kind (KindedTV _ kind) = kind

ty_var_name :: TyVarBndr -> Name
ty_var_name (KindedTV name _ ) = name
ty_var_name (PlainTV name) = name

set_ty_var_name :: Name -> TyVarBndr -> TyVarBndr
set_ty_var_name n (KindedTV _ k) = KindedTV n k

group_by_kind :: [TyVarBndr] -> [[TyVarBndr]]
group_by_kind tyvars = groupBy (\x y -> ty_var_kind x == ty_var_kind y ) tyvars

possibly_share :: Bool -> [TyVarBndr] -> [TyVarBndr]
--possibly_share True (x:xs) = result where
--    name   = ty_var_name x
--    new_xs = map (set_ty_var_name name) xs
--   result = x:xs
possibly_share _ x = x

possibly_make_plain :: Bool -> TyVarBndr -> TyVarBndr
possibly_make_plain _ (KindedTV n _) = PlainTV n
--possibly_make_plain _ x = x

dec_to_typ :: Dec -> Type
dec_to_typ (DataD _ name ty_vars _ _) = foldl' AppT (ConT name) $ map (VarT . ty_var_name) ty_vars
dec_to_typ (NewtypeD _ name ty_vars _ _) = foldl' AppT (ConT name) $ map (VarT . ty_var_name) ty_vars

--type KindContext = ReaderT KindEnv Gen

haskell_98_data_dec :: [Dec] -> Gen Dec
haskell_98_data_dec other_types = do
    --all of the dec must be explictly kinded!
    name <- arb_constructor_name
    ty_var_size <- choose (0, 10)
    typ_vars    <- vectorOf ty_var_size (elements letter_strings)
                  
    concre_size <- choose (0, 10)
    concre_typs <- vectorOf concre_size (elements other_types)
    
    let renamed_concre_decs = map prefix_ty_vars concre_typs
        renamed_concre_typs = map dec_to_typ renamed_concre_decs
        all_ty_var_bndrs    = nub $ concatMap collect_kinded_tyvars renamed_concre_decs
        grouped = group_by_kind all_ty_var_bndrs
    
    should_share <- vectorOf (length grouped) arbitrary
        
    let shared  = concat $ zipWith possibly_share should_share grouped
    
    should_make_plain <- vectorOf (length shared) arbitrary

    let ty_var_bndrs  = zipWith possibly_make_plain should_make_plain shared
        typ_var_names = map ty_var_name ty_var_bndrs    
        var_types     = map VarT typ_var_names
    cons <- if length renamed_concre_typs > 0 
               then mapM arb_con_with_types =<< small_list_of (small_list_of $ elements (renamed_concre_typs))  -- ++ var_types
               else (:[]) <$> arb_con_with_types []
    index        <- choose(0, length cons - 1)
    
    return $ DataD [] name ty_var_bndrs cons [] 
    
arb_field_name = do
        x <- lowerAlpha
        name_size <- choose (10, 30)
        xs <- vectorOf name_size (oneof [numeric, lowerAlpha, upperAlpha])
        return $ mkName $ x:xs

arb_con_with_types :: [Type] -> Gen Con
arb_con_with_types xs = do
    name <- arb_constructor_name
    ss <- vectorOf (length xs) $ oneof [return IsStrict, return NotStrict]
    option <- choose (0, 1 :: Int)
    case option of
        0 -> return $ NormalC name $ zip ss xs
        1 -> do
             field_names <- vectorOf (length xs) arb_field_name
             let var_strict_types = zip3 field_names ss xs
             return $ RecC name var_strict_types
             


primitive_types = $(L.lift =<< mapM explictly_kind [''Int, ''Bool, ''Char, ''Float, ''[]])
    

small_list_of elems = do
    size <- choose (0, 10)
    vectorOf size elems
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
            