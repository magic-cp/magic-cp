--
-- (c) Susumu Katayama
--
{-# OPTIONS -cpp #-}
module MagicHaskeller.ReadTHType(thTypeToType, typeToTHType, showTypeName, MagicHaskeller.ReadTHType.plainTV, unPlainTV) where

import MagicHaskeller.Types as Types
import MagicHaskeller.TyConLib
import Language.Haskell.TH as TH
import Control.Monad.State.Lazy
import Data.Array(Array, (!), inRange, bounds)
import Data.Char(ord,chr)
import Data.List(nub)
import Data.Map(lookup)

import Debug.Trace

showTypeName = TH.nameBase -- Use the unqualified name to avoid confusion because Data.Typeable.tyConString shows the unqualified name for types defined in the Standard Hierarchical Library (though the qualified name is shown when Typeable is derived).
-- showTypeName = show -- maybe in future, when TypeRep can be shown qualified.

-- MyDynamicでしか使われていないので，ForallTは単に無視する．PolyDynamicのチェックがちょっと緩くなるだけ．
thTypeToType :: TyConLib -> TH.Type -> Types.Type
thTypeToType tcl t = trace (show t ++ "\n ---------\n"
                        ++ show tcl ++ "\n ----------\n") $
                      normalize $ evalState (thTypeToType' tcl t) []

thTypeToType' :: TyConLib -> TH.Type -> State [Name] Types.Type
thTypeToType' tcl (ForallT bs []    t) = do
  modify (\vs -> vs++map tyVarBndrToName bs)
  thTypeToType' tcl t
thTypeToType' tcl (ForallT _ (_:_) t) = error "Type classes are not supported yet."
thTypeToType' tcl (TupleT n)      = return $ TC (tuple tcl n)
thTypeToType' tcl ListT           = return $ TC (list tcl)
-- thTypeToType' tcl (HsTyFun ht0 ht1)
--     = thTypeToType' tcl ht0 :-> thTypeToType' tcl ht1
thTypeToType' tcl (AppT (AppT ArrowT      ht0) ht1) = do
  t1 <- thTypeToType' tcl ht0
  t2 <- thTypeToType' tcl ht1
  return $ t1 :-> t2
thTypeToType' tcl (AppT (AppT (ConT name) ht0) ht1) | nameBase name == "(->)" = do
  t1 <- thTypeToType' tcl ht0
  t2 <- thTypeToType' tcl ht1
  return $ t1 :> t2
thTypeToType' tcl (AppT ht0 ht1) = do
  t1 <- thTypeToType' tcl ht0
  t2 <- thTypeToType' tcl ht1
  return $ TA t1 t2
thTypeToType' (fm,_) (ConT name) = let nstr = showTypeName name
                                   in case Data.Map.lookup nstr fm of
                                          Nothing -> -- TC $ (-1 - bakaHash nstr)
                                                     error $ "thTypeToType' : "++nstr++" : unknown TyCon"
                                          Just c  -> return $ TC c
{- この辺は単なるコメントアウトでいいんだっけ？
thTypeToType' tcl (HsTyCon (Special HsUnitCon)) = TC (unit tcl)
thTypeToType' tcl (HsTyCon (Special HsListCon)) = TC (list tcl)
-}
thTypeToType' _  ArrowT = error "Partially applied (->)."
thTypeToType' _  (VarT name) = do
                                  modify $ \vs -> case Prelude.lookup name $ zip vs [0..] of
                                                    Nothing -> name : vs
                                                    _ -> vs
                                  vs <- get
                                  trace (show vs) (return ())
                                  return $ TV $ case Prelude.lookup name $ zip vs [0..] of
                                                Nothing -> error "thTypeToType : unbound type variable"
                                                Just i  -> i
-- thTypeToType' _   hst = error ("thTypeToType': "++show hst)

tyVarBndrToName (PlainTV name)    = name
tyVarBndrToName (KindedTV name _) = name

{- tcKindを廃止するので．ま，tcKindがなくてもhigher-order kindでなければトップレベルのkindから推論できるし．
-- copied from svn/MagicHaskeller/memodeb/RandomFilter.hs
typeToTHType :: TyConLib -> Types.Type -> TH.Type
typeToTHType tcl ty = TH.ForallT (map tvToName $ tyvars ty) [] (typeToTHType' tcl ty)
typeToTHType' (_,ar) (TC tc) | tcid >= 0 = TH.ConT (TH.mkName $ fst ((ar ! tcKind tc) !! tcid))
                             where tcid = tcID tc
typeToTHType' tcl    (TV tv) = TH.VarT $ tvToName tv
typeToTHType' tcl (TA t0 t1) = TH.AppT (typeToTHType' tcl t0) (typeToTHType' tcl t1)
typeToTHType' tcl (t0:->t1)  = TH.AppT (TH.AppT TH.ArrowT (typeToTHType' tcl t0)) (typeToTHType' tcl t1)
typeToTHType' tcl (t0:> t1)  = TH.AppT (TH.AppT sectionedArrow (typeToTHType' tcl t0)) (typeToTHType' tcl t1)
tvToName = TH.mkName . return . chr . (+ ord 'a') . tvID
-}
typeToTHType :: TyConLib -> Types.Type -> TH.Type
typeToTHType tcl ty = (case tyvars ty of []  -> id
                                         tvs -> TH.ForallT (map (MagicHaskeller.ReadTHType.plainTV . tvToName) $ nub tvs) [])
                                                               (typeToTHType' tcl 0 ty)
typeToTHType' (_,ar) k (TC tc) | tc >= 0 = if name == "[]" then ListT else TH.ConT (TH.mkName name)
                             where name = if inRange (bounds ar) k then fst ((ar ! k) !! fromIntegral tc)
                                                                   else 'K':shows k ('I':show tc) -- useful with defaultTCL
typeToTHType' tcl    _ (TV tv) = TH.VarT $ tvToName tv
typeToTHType' tcl    k (TA t0 t1) = TH.AppT (typeToTHType' tcl (k+1) t0) (typeToTHType' tcl 0 t1)
typeToTHType' tcl    0 (t0:->t1)  = TH.AppT (TH.AppT TH.ArrowT (typeToTHType' tcl 0 t0)) (typeToTHType' tcl 0 t1)
typeToTHType' tcl    0 (t0:> t1)  = TH.AppT (TH.AppT sectionedArrow (typeToTHType' tcl 0 t0)) (typeToTHType' tcl 0 t1)
-- tvToName = TH.mkName . return . chr . (+ ord 'a')

-- Maybe this should be dealt with by the version number of template-haskell, but how can I tell the number in the source code?
#if __GLASGOW_HASKELL__<=610
plainTV = id
unPlainTV = id
#else
plainTV = PlainTV
unPlainTV (PlainTV v) = v
unPlainTV (KindedTV v _) = v -- Uh, are there be problems?
#endif
tvToName n = TH.mkName ('t':show n)

-- secionedArrow = TH.ConT ''(->) -- 多分こっちでもOK
sectionedArrow = TH.ConT (mkName "GHC.Prim.(->)")

{-
Prelude> :module +Language.Haskell.TH
Prelude Language.Haskell.TH> AppT (AppT (ConT $ mkName "GHC.Prim.(->)") (ConT $ mkName "GHC.Base.Int")) (ConT $ mkName  "Char")
AppT (AppT (ConT GHC.Prim.(->)) (ConT GHC.Base.Int)) (ConT Char)
Prelude Language.Haskell.TH> AppT (AppT ArrowT (ConT $ mkName "GHC.Base.Int")) (ConT $ mkName  "Char")
AppT (AppT ArrowT (ConT GHC.Base.Int)) (ConT Char)
-}
