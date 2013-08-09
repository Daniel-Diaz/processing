
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell, i.e. where magic happens.
module Graphics.Web.Processing.Core.TH (
    deriveRecursive
  , procTypeMechs
  , deriveProcTypeInsts
  ) where

import Language.Haskell.TH

{- RECURSOR -}

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

{- PROC TYPES -}

-- | List of the Proc_* types which can be used as arguments, set into variables, etc.
procTypeNames :: [String]
procTypeNames = [ "Bool", "Int", "Float", "Image", "Text", "Char" ]

dataProcArg :: Dec
dataProcArg = DataD [] (mkName "ProcArg") [] (fmap cons procTypeNames) [mkName "Eq",mkName "Generic"]
  where
    cons x = NormalC (mkName $ x ++ "Arg") [(NotStrict,ConT $ mkName $ "Proc_" ++ x)]

dataProcAssign :: Dec
dataProcAssign = DataD [] (mkName "ProcAssign") [] (fmap cons procTypeNames) [mkName "Eq",mkName "Generic"]
  where
    cons x = NormalC (mkName $ x ++ "Assign")
         [ (NotStrict,ConT $ mkName "Text")
         , (NotStrict,ConT $ mkName $ "Proc_" ++ x)]

-- | Create 'ProcType' instance for a @Proc_*@ type.
procTypeInst :: String -> Dec
procTypeInst n = InstanceD [] (AppT (ConT $ mkName "ProcType") $ ConT $ mkName $ "Proc_" ++ n)
  [ FunD (mkName "proc_asign") [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Assign") [] ]
  , FunD (mkName "proc_arg"  ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Arg"  ) [] ]
  , FunD (mkName "proc_read" ) [ Clause [ConP (mkName "Var") [VarP $ mkName "v"]]
                               ( NormalB $ AppE (ConE $ mkName $ n ++ "_Var")
                                                (VarE $ mkName "v") )
                               [] ]
  , FunD (mkName "proc_cond" ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "_Cond") [] ]
    ]

-- | Pretty instance of ProcArg.
procArgPrettyInst :: Dec -> Dec
procArgPrettyInst procArg =
  let DataD _ _ _ cs _ = procArg
      defs = fmap (\(NormalC n _) ->
               Clause [ConP n [VarP $ mkName "x"]]
                              (NormalB $ AppE (VarE $ mkName "ppr")
                                              (VarE $ mkName "x"  ) ) [] ) cs
      inst = FunD (mkName "ppr") defs
  in  InstanceD [] (AppT (ConT $ mkName "Pretty") (ConT $ mkName "ProcArg")) [inst]

-- | Pretty instance of ProcArg.
procAssignPrettyInst :: Dec -> Dec
procAssignPrettyInst procAssign =
  let DataD _ _ _ cs _ = procAssign
      defs = fmap (\(NormalC n _) ->
                 let t = VarE $ mkName "t"
                     x = VarE $ mkName "x"
                     e1 <+> e2 = InfixE (Just e1) (VarE $ mkName "<+>") (Just e2)
                     fromText = AppE $ VarE (mkName "fromText")
                     e = fromText t <+> fromText (LitE $ StringL "=") <+> AppE (VarE $ mkName "ppr") x
                 in  Clause [ConP n [VarP $ mkName "t", VarP $ mkName "x"]] (NormalB e) []
                  ) cs
      inst = FunD (mkName "ppr") defs
  in  InstanceD [] (AppT (ConT $ mkName "Pretty") (ConT $ mkName "ProcAssign")) [inst]

procTypeMechs :: Q [Dec]
procTypeMechs =
 -- ProcArg Pretty instance
 let argp = procArgPrettyInst dataProcArg
 -- ProcAssign Pretty instance
     assignp = procAssignPrettyInst dataProcAssign
 -- Everything
 in  return $ [ dataProcArg , argp , dataProcAssign , assignp ]

deriveProcTypeInsts :: Q [Dec]
deriveProcTypeInsts = return $ fmap procTypeInst procTypeNames
