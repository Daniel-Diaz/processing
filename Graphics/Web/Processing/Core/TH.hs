
-- | Template Haskell derivations.
module Graphics.Web.Processing.Core.TH (
    procTypeInst
  , procArgPrettyInst
  , deriveRecursive
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

-- | Pretty instance of ProcArg.
procArgPrettyInst :: Q [Dec]
procArgPrettyInst = do
  (TyConI (DataD _ _ _ cs _)) <- reify $ mkName "ProcArg"
  let defs = fmap (\(NormalC n _) ->
               Clause [ConP n [VarP $ mkName "x"]]
                              (NormalB $ AppE (VarE $ mkName "ppr")
                                              (VarE $ mkName "x"  ) ) [] ) cs
      inst = FunD (mkName "ppr") defs
  return . return $ InstanceD [] (AppT (ConT $ mkName "Pretty") (ConT $ mkName "ProcArg")) [inst]

-- | Define recursor over a data type. The recursor will apply an inner function
--   over subexpressions of the same type.
defineRecursor :: Name -> Q Dec
defineRecursor t = do
  (TyConI (DataD _ _ _ cs _)) <- reify t
  let cs' = filter (\(NormalC _ args_) ->
             let args = fmap snd args_
             in  elem (ConT t) args
                     ) cs
  binds <- mapM (\(NormalC n args_) -> do
             let args = fmap snd args_
             vars <- mapM (const $ newName "x") args
             return $ Clause [VarP (mkName "f") , ConP n (fmap VarP vars)]
                             (NormalB $ foldl AppE (ConE n) $
                                        zipWith (\v a -> if a == ConT t
                                                            then AppE (VarE $ mkName "f") (VarE v)
                                                            else VarE v
                                                  ) vars args)
                             []
                  ) cs'
  let lastbind = Clause [WildP,VarP $ mkName "x"] (NormalB $ VarE $ mkName "x") []
      fname = mkName "recursor"
  return $ FunD fname $ binds ++ [lastbind]

-- | Automatic derivation of Recursive class, using 'defineRecursor'.
deriveRecursive :: Name -> Q [Dec]
deriveRecursive t = do
  r <- defineRecursor t
  return . return $ InstanceD [] (AppT (ConT $ mkName "Recursive") (ConT t)) [r]
