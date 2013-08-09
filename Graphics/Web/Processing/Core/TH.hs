
module Graphics.Web.Processing.Core.TH (
  procTypeInst
  ) where

import Language.Haskell.TH

-- | Create 'ProcType' instance for a @Proc_*@ type.
procTypeInst :: String -> Q [Dec]
procTypeInst n = return . return $ InstanceD [] (AppT (ConT $ mkName "ProcType") $ ConT $ mkName $ "Proc_" ++ n)
  [ FunD (mkName "proc_asign") [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Asign") [] ]
  , FunD (mkName "proc_arg"  ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Arg"  ) [] ]
  , FunD (mkName "proc_read" ) [ Clause [ConP (mkName "Var") [VarP $ mkName "v"]]
                               ( NormalB $ AppE (ConE $ mkName $ n ++ "_Var")
                                                (VarE $ mkName "v") )
                               [] ]
  , FunD (mkName "proc_cond" ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "_Cond") [] ]
    ]
