{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{- | A grab bag of useful instances for Template Haskell types -}
module Language.Haskell.TH.LiftInstances where
    
import qualified Language.Haskell.TH.Lift as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--import Language.Haskell.TH.LambdaConvert
import Language.Haskell.TH.KindInference

$(L.deriveLiftMany [''Match, ''Range, ''Stmt, ''Strict,
                  ''InlineSpec,
                  ''Safety,
                  ''Callconv,
                  ''Guard,
                  ''Lit,
                  ''Exp, ''FixityDirection, ''Clause, ''Pat,
                  ''Body, ''FunDep, ''Foreign,
                  ''Pragma, ''FamFlavour, ''TyVarBndr,
                  ''Kind, ''Pred, ''Con, 
                  ''Dec, ''ClassInstance, ''Type, ''Fixity, ''Info])
                  
un_ty_con (TyConI x) = x

explictly_kind name = do
    Right kind      <- inferKind name
    TyConI data_dec <- reify name
    return $ convert_ty_vars data_dec kind
    
    
convert_ty_vars :: Dec -> Kind -> Dec
convert_ty_vars (DataD x y ty_vars z w) kind    = DataD x y (replace_ty_vars ty_vars kind) z w
convert_ty_vars (NewtypeD x y ty_vars z w) kind = NewtypeD x y (replace_ty_vars ty_vars kind) z w
 
replace_ty_vars ty_vars kind = result where
    kinds = unfold_kinds kind (length ty_vars)
    result = zipWith add_kind ty_vars kinds
    
unfold_kinds x y = unfold_kinds' x y []
    
unfold_kinds' _ 0        accum = accum
unfold_kinds' (ArrowK x y) i accum = unfold_kinds' y (i - 1) (x:accum)

add_kind (PlainTV n) k = KindedTV n k
add_kind x k = x

