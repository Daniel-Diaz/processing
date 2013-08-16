
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell, i.e. where magic happens.
module Graphics.Web.Processing.Core.TH (
    deriveRecursive
  , procTypeMechs
  , deriveProcTypeInsts
  , deriveCustomValues
  , deriveOptimizable
  ) where

import Language.Haskell.TH
import Control.Monad
import Data.Maybe (catMaybes)
import Data.List (isSuffixOf)

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

-- | Return the actual name of a type used in processing.js code.
realName :: String -> String
realName "Bool" = "boolean"
realName "Int" = "int"
realName "Float" = "float"
realName "Image" = "PImage"
realName "Text" = "String"
realName "Char" = "char"
realName _ = "undefined"

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

ptype :: [Dec]
ptype = [
    SigD (mkName "ptype") $ AppT ArrowT (ConT $ mkName "ProcAssign") `AppT` (ConT $ mkName "Doc")
  , FunD (mkName "ptype") $ fmap cons procTypeNames
    ]
 where
   cons x = Clause [ConP (mkName $ x ++ "Assign") [WildP,WildP]]
                   (NormalB $ AppE (VarE $ mkName "fromText") $ LitE $ StringL $ realName x) []

ltype :: [Dec]
ltype = [
    SigD (mkName "ltype") $ AppT ArrowT (ConT $ mkName "ProcList") `AppT` (ConT $ mkName "Doc")
  , FunD (mkName "ltype") $ fmap cons procTypeNames
    ]
 where
   cons x = Clause [ConP (mkName $ x ++ "List") [WildP]]
                   (NormalB $ AppE (VarE $ mkName "fromText") $ LitE $ StringL $ realName x ++ "[]") []

dataProcList :: Dec
dataProcList = DataD [] (mkName "ProcList") [] (fmap cons procTypeNames) [mkName "Eq",mkName "Generic"]
  where
    cons x = NormalC (mkName $ x ++ "List") [(NotStrict,AppT ListT $ ConT $ mkName $ "Proc_" ++ x)]

-- | Pretty instance of ProcList.
procListPrettyInst :: Dec -> Dec
procListPrettyInst procList =
  let DataD _ _ _ cs _ = procList
      _fmap e1 e2 = AppE (VarE $ mkName "fmap") e1 `AppE` e2
      _ppr  = VarE $ mkName "ppr"
      _xs = VarE $ mkName "xs"
      _fromText = AppE (VarE $ mkName "fromText")
      _commasep = AppE (VarE $ mkName "commasep")
      e1 <> e2 = InfixE (Just e1) (VarE $ mkName "<>") (Just e2)
      leftbr  = _fromText $ LitE $ StringL "{"
      rightbr = _fromText $ LitE $ StringL "}"
      defs = fmap (\(NormalC n _) ->
               Clause [ConP n [VarP $ mkName "xs"]]
                              (NormalB $ leftbr <> (_commasep $ _fmap _ppr _xs) <> rightbr)
                              []
                    ) cs
      inst = FunD (mkName "ppr") defs
  in  InstanceD [] (AppT (ConT $ mkName "Pretty") (ConT $ mkName "ProcList")) [inst]

-- | Create 'ProcType' instance for a @Proc_*@ type,
--   given the function declaration of checkForArg.
procTypeInst :: String -> Dec -> Dec
procTypeInst n cfa = InstanceD [] (AppT (ConT $ mkName "ProcType") $ ConT $ mkName $ "Proc_" ++ n)
  [ FunD (mkName "proc_assign") [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Assign") [] ]
  , FunD (mkName "proc_list") [ Clause [] (NormalB $ ConE $ mkName $ n ++ "List") [] ]
  , FunD (mkName "proc_arg"  ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "Arg"  ) [] ]
  , FunD (mkName "proc_read" ) [ Clause [ConP (mkName "Var") [VarP $ mkName "v"]]
                               ( NormalB $ AppE (ConE $ mkName $ n ++ "_Var")
                                                (VarE $ mkName "v") )
                               [] ]
  , FunD (mkName "proc_cond" ) [ Clause [] (NormalB $ ConE $ mkName $ n ++ "_Cond") [] ]
  , cfa
    ]

(||*) :: Exp -> Exp -> Exp
e1 ||* e2 = InfixE (Just e1) (VarE $ mkName "||") (Just e2)

checkForVar :: String -> Q Dec
checkForVar t = do
 TyConI (DataD _ _ _ cs _) <- reify $ mkName $ "Proc_" ++ t
 ds <- sequence 
   [ do vs <- mapM (\(ConT a) -> if elem (nameBase a) $ fmap ("Proc_"++) procTypeNames
                                    then fmap Just $ newName "x"
                                    else return Nothing) $ fmap snd as
        let patf Nothing  = WildP
            patf (Just v) = VarP v
            bodyf v = VarE (mkName "checkForVar") `AppE` VarE (mkName "t") `AppE` VarE v
            vs' = catMaybes vs
        return $ Clause [if null vs' then WildP else VarP $ mkName "t" , ConP n $ fmap patf vs]
                        (NormalB $ foldr (\x y -> bodyf x ||* y) (ConE $ mkName "False") vs')
                        []
     | NormalC n as <- cs
     , let str = nameBase n
     , str /= t ++ "_Var"
       ]
 b <- [| $(dyn "t") == $(dyn "v") |]
 let d = Clause [VarP $ mkName "t" , ConP (mkName $ t ++ "_Var") [VarP $ mkName "v"]]
                (NormalB b)
                []
 return $ FunD (mkName "checkForVar") $ d : ds

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
 -- ProcList Pretty instance
     listp = procListPrettyInst dataProcList
 -- Everything
 in  return $ [ dataProcArg , argp
              , dataProcAssign , assignp
              , dataProcList , listp ] ++ ptype ++ ltype

deriveProcTypeInsts :: Q [Dec]
deriveProcTypeInsts = fmap (++isVarIn) $ mapM (
  \t -> do d <- checkForVar t
           return $ procTypeInst t d
    ) procTypeNames

isVarIn :: [Dec]
isVarIn = isVarInArg ++ isVarInAssign ++ assignVarName

-- isVarInArg :: Text -> ProcArg -> Bool
isVarInArg :: [Dec]
isVarInArg = [ SigD (mkName "isVarInArg") $ textt ->. argt ->. boolt
             , FunD (mkName "isVarInArg") $ fmap f procTypeNames ]
  where
   textt = ConT $ mkName "Text"
   argt  = ConT $ mkName "ProcArg"
   boolt = ConT $ mkName "Bool"
   f t = Clause [VarP $ mkName "t" , ConP (mkName $ t ++ "Arg") [VarP $ mkName "x"]]
                (NormalB $ VarE (mkName "checkForVar") `AppE` VarE (mkName "t") `AppE` VarE (mkName "x"))
                []

-- isVarInAssign :: Text -> ProcAssign -> Bool
isVarInAssign :: [Dec]
isVarInAssign = [ SigD (mkName "isVarInAssign") $ textt ->. argt ->. boolt
                , FunD (mkName "isVarInAssign") $ fmap f procTypeNames ]
  where
   textt = ConT $ mkName "Text"
   argt  = ConT $ mkName "ProcAssign"
   boolt = ConT $ mkName "Bool"
   f t = Clause [VarP $ mkName "t" , ConP (mkName $ t ++ "Assign") [WildP, VarP $ mkName "x"]]
                (NormalB $ VarE (mkName "checkForVar") `AppE` VarE (mkName "t") `AppE` VarE (mkName "x"))
                []

-- assignVarName :: ProcAssign -> Text
assignVarName :: [Dec]
assignVarName = [ SigD (mkName "assignVarName") $ ConT (mkName "ProcAssign") ->. ConT (mkName "Text")
                , FunD (mkName "assignVarName") $ fmap f procTypeNames ]
  where
   f t = Clause [ConP (mkName $ t ++ "Assign") [VarP $ mkName "t",WildP]]
                (NormalB $ VarE $ mkName "t") []

infixr 4 ->.

(->.) :: Type -> Type -> Type
t1 ->. t2 = ArrowT `AppT` t1 `AppT` t2

-- CUSTOM VALUES

deriveCustomValues :: Q [Dec]
deriveCustomValues = do
  let xs = fmap varLengthInst procTypeNames
  ys <- mapM customValueInst procTypeNames
  return $ xs ++ ys

varLengthInst :: String -> Dec
varLengthInst t = InstanceD [] (AppT (ConT $ mkName "VarLength") (ConT $ mkName $ "Proc_" ++ t)) [
  FunD (mkName "varLength") [ Clause [WildP] (NormalB $ LitE $ IntegerL 1) [] ]
  ]

customValueInst :: String -> Q Dec
customValueInst t = instanceD (return []) [t|$(conT $ mkName "CustomValue") $(conT $ mkName $ "Proc_" ++ t)|]
  [ funD (mkName "newVarC")
      [ do b <- fmap NormalB $ [|liftM $(dyn "fromVar") . $(dyn "newVar")|]
           return $ Clause [] b []
      ]
  , funD (mkName "newArrayVarC")
      [ do b <- fmap NormalB $ [|liftM $(dyn "fromArrayVar") . $(dyn "newArrayVar")|]
           return $ Clause [] b []
      ]
  , funD (mkName "readVarC")
      [ do b <- fmap NormalB $ [|$(dyn "readVar") . head . $(dyn "fromCustomVar")|]
           return $ Clause [] b []
      ]
  , funD (mkName "writeVarC")
      [ do b <- fmap NormalB $ [|$(dyn "writeVar") (head $ $(dyn "fromCustomVar") $(dyn "v")) $(dyn "x")|]
           return $ Clause [VarP (mkName "v"),VarP (mkName "x")] b []
      ]
  , funD (mkName "ifC")
      [ return $ Clause [] (NormalB $ VarE $ mkName "if_") []
      ]
    ]

-- OPTIMIZATION CLASS

optimizableTypes :: [String]
optimizableTypes = [ "Bool", "Int", "Float" ]

deriveOptimizable :: Q [Dec]
deriveOptimizable = mapM optimizableInst optimizableTypes

optimizableInst :: String -> Q Dec
optimizableInst tn = do
  let t  = mkName $ "Proc_" ++ tn
      ts = [ mkName $ "Proc_" ++ str
           | str <- optimizableTypes , str /= tn ]
  -- browse*
  TyConI (DataD _ _ _ cs _) <- reify t
  selfds  <- sequence [ browseSelf n $ fmap snd as
                      | NormalC n as <- cs ]
  let browseSelfD = FunD (mkName $ "browse" ++ tn) selfds
  browseOthersD <-
      mapM (\ot -> do TyConI (DataD _ _ _ ocs _) <- reify ot
                      otherds <- sequence [ browseOther n $ fmap snd as
                                          | NormalC n as <- ocs ]
                      return $ FunD (mkName $ "browse" ++ (drop 5 $ nameBase ot)) otherds
            ) ts
  -- numOps
  numOpsClauses <- sequence [ numOpsC n $ fmap snd as
                            | NormalC n as <- tail cs
                            , not $ null as
                            , let str = nameBase n
                            , not $ "Random" `isSuffixOf` str
                            , not $ "Var" `isSuffixOf` str
                              ]
  let numOpsD = FunD (mkName "numOps") $ numOpsClauses
                                      ++ [Clause [WildP] (NormalB $ LitE $ IntegerL 0) []]
  -- ReplaceIn*
  let idClause = Clause [WildP,WildP,VarP $ mkName "e"] (NormalB $ VarE $ mkName "e") []
  replaceInSelfCs <- sequence [ replaceInSelfC n $ fmap snd as
                              | NormalC n as <- tail cs
                              , not $ null as
                              , let str = nameBase n
                              , not $ "Random" `isSuffixOf` str
                              , not $ "Var" `isSuffixOf` str
                                ]
  let replaceInSelf = FunD (mkName $ "replaceIn" ++ tn) $ replaceInSelfCs ++ [idClause]
  replaceInOthers <-
      mapM (\ot -> do TyConI (DataD _ _ _ ocs _) <- reify ot
                      othersd <- sequence [ replaceInOtherC n $ fmap snd as
                                          | NormalC n as <- tail ocs
                                          , not $ null as
                                          , let str = nameBase n
                                          , not $ "Random" `isSuffixOf` str
                                          , not $ "Var" `isSuffixOf` str
                                            ]
                      return $ FunD (mkName $ "replaceIn" ++ (drop 5 $ nameBase ot)) $ othersd ++ [idClause]
            ) ts
  -- Return
  return $ InstanceD [] (ConT (mkName "Optimizable") `AppT` ConT t) $
      (numOpsD : browseSelfD : browseOthersD) ++
      (replaceInSelf : replaceInOthers)

replaceInOtherC :: Name -> [Type] -> Q Clause
replaceInOtherC c ts = do
  vs <- mapM (\t -> fmap (\v -> (v,t)) $ newName "x") ts
  let patf (v,_) = VarP v
      bodyf (v,ConT t) = let str = nameBase t
                         in  if str `elem` fmap ("Proc_"++) optimizableTypes
                                then VarE (mkName $ "replaceIn" ++ drop 5 str)
                                       `AppE` VarE (mkName "o") 
                                       `AppE` VarE (mkName "t")
                                       `AppE` VarE v
                                else VarE v
      bodyf _ = error "TH.ReplaceInOther: Bad constructor. Report this as a bug."
      e2 = foldl1 AppE $ ConE c : fmap bodyf vs
      optts = filter (\(ConT t) -> nameBase t `elem` fmap ("Proc_"++) optimizableTypes) ts
      cleanf x = if null optts then WildP else x
  return $ Clause [ cleanf $ VarP $ mkName "o" -- Origin variable
                  , cleanf $ VarP $ mkName "t" -- Target variable
                  , ConP c $ fmap patf vs]
                  (NormalB e2)
                  []

replaceInSelfC :: Name -> [Type] -> Q Clause
replaceInSelfC c ts = do
  vs <- mapM (\t -> fmap (\v -> (v,t)) $ newName "x") ts
  let patf (v,_) = VarP v
  b <- [|$(dyn "o") == $(dyn "e")|]
  let e1 = VarE $ mkName "t"
      bodyf (v,ConT t) = let str = nameBase t
                         in  if str `elem` fmap ("Proc_"++) optimizableTypes
                                then VarE (mkName $ "replaceIn" ++ drop 5 str)
                                       `AppE` VarE (mkName "o") 
                                       `AppE` VarE (mkName "t")
                                       `AppE` VarE v
                                else VarE v
      bodyf _ = error "TH.ReplaceInSelf: Bad constructor. Report this as a bug."
      e2 = foldl1 AppE $ ConE c : fmap bodyf vs
  return $ Clause [ VarP $ mkName "o" -- Origin variable
                  , VarP $ mkName "t" -- Target variable
                  , AsP (mkName "e") $ ConP c $ fmap patf vs]
                  (NormalB $ CondE b e1 e2)
                  []

(>>>) :: Exp -> Exp -> Exp
e1 >>> e2 = InfixE (Just e1) (VarE $ mkName ">>") (Just e2)

returnu :: Exp
returnu = VarE (mkName "return") `AppE` TupE []

optimizableVars :: [Type] -- Types
                -> Q [Maybe (Name,String)] -- List of pairs (newVar,Proc_* optimizable type without Proc_)
optimizableVars ts = sequence [ if n `elem` fmap ("Proc_"++) optimizableTypes
                                   then do v <- newName "x"
                                           return $ Just (v,drop 5 n)
                                   else return Nothing
                              | ConT t <- ts
                              , let n = nameBase t
                                ]

browseOther :: Name   -- Constructor name
            -> [Type] -- Types of the constructor arguments
            -> Q Clause -- Clause of the browse* definition for
                        -- the given constructor
browseOther c ts = do
   vs <- optimizableVars ts
   let patf Nothing  = WildP
       patf (Just (v,_)) = VarP v
       bodyf (v,t) = VarE (mkName $ "browse" ++ t) `AppE` VarE v
   return $ Clause [ConP c $ fmap patf vs]
                   (NormalB $ foldr (>>>) returnu $ fmap bodyf $ catMaybes vs)
                   []

browseSelf :: Name -> [Type] -> Q Clause
browseSelf c ts = do
 Clause p (NormalB b) d <- browseOther c ts
 return $ Clause (fmap (AsP $ mkName "e") p)
                 (NormalB $ AppE (VarE $ mkName "addExp")
                                 (VarE $ mkName "e") >>> b) d

(+.) :: Exp -> Exp -> Exp
e1 +. e2 = InfixE (Just e1) (VarE $ mkName "+") (Just e2)

oneE :: Exp
oneE = LitE $ IntegerL 1

numOpsC :: Name -- Constructor name
        -> [Type] -- Types of the constructor arguments (non-empty list)
        -> Q Clause
numOpsC c ts = do
  vs <- optimizableVars ts
  let patf Nothing = WildP
      patf (Just (v,_)) = VarP v
      bodyf (v,_) = VarE (mkName "numOps") `AppE` VarE v
  return $ Clause [ConP c $ fmap patf vs]
                  (NormalB $ foldl (+.) oneE $ fmap bodyf $ catMaybes vs)
                  []
