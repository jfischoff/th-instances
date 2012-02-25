module Language.Haskell.TH.LambdaConvert where
import qualified Language.Lambda.SimplyTyped.Let.Syntax as L
import qualified Language.Haskell.TH as TH
{-

the plan is to convert lambdas to data declarations 

so first what are the base types?

lets say *

so I get a typed expression back
now how do I convert?

well Var -> VarT
lambdas become ConT
I just need to make up names
the only issue is I don't have names for my functions

Now that I am coming back to this I am confused

-}

data Star = Star 
    deriving(Show, Eq)
    
data DataConstant = DataConstant

to_data_dec :: L.Expr T.Name (L.Type Star) DataConstant -> TH.Dec
to_data_dec (Lam sym ) = undefined
