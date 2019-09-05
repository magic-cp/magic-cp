--
-- (c) Susumu Katayama
--
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -cpp #-}
module MagicHaskeller.ProgGen(ProgGen(PG), mkCL, ClassLib(..), mguPrograms) where

import MagicHaskeller.Types
import MagicHaskeller.TyConLib
import Control.Monad
import Data.Monoid
import MagicHaskeller.CoreLang
import Control.Monad.Search.Combinatorial
import MagicHaskeller.PriorSubsts
import Data.List(partition, sortBy, genericLength)
import Data.Ix(inRange)

import MagicHaskeller.ProgramGenerator
import MagicHaskeller.Options(Opt(..))

import MagicHaskeller.Classify
import MagicHaskeller.Instantiate

import MagicHaskeller.Expression hiding ((<$>))

import MagicHaskeller.T10
import qualified Data.Map as Map

import MagicHaskeller.DebMT

import Debug.Trace

import MagicHaskeller.MemoToFiles hiding (freezePS,fps)

--import System.IO.Unsafe(unsafePerformIO)
--import Data.IORef

traceTy _    = id
--traceTy fty = trace ("lookup "++ show fty)


type BF = Recomp
-- type BF = DBound

type BFM = Matrix
-- type BFM = DBMemo
fromMemo :: Search m => Matrix a -> m a
fromMemo = fromMx
toMemo :: Search m => m a -> Matrix a
toMemo = toMx


-- Memoization table, created from primitive components

-- | The vanilla program generator corresponding to Version 0.7.*
newtype ProgGen = PG (MemoDeb (ClassLib CoreExpr) CoreExpr) -- ^ internal data representation
newtype ClassLib e = CL (MemoDeb (ClassLib e) e)
-- mapStrTyCon :: Search m => MemoDeb c m CoreExpr -> Map.Map String TyCon
-- mapStrTyCon = fst . extractTCL . PG

type MemoTrie a = MapType (BFM (Possibility a))

lmt :: MapType a -> Type -> a
lmt mt fty =
       traceTy fty $
       lookupMT mt fty

lookupFunsShared :: (Search m) => Generator m CoreExpr
lookupFunsShared memodeb@(_,mt,_,_) avail reqret
    = let annAvails = zip [0..] avail -- [(0, targ2), (1, targ1), (2, targ0)]
      in
        PS (\subst mx ->
        fromRc $ Rc $ \d -> -- d+1 is the maximum number of arguments to use.
        concat [ let (tn, decoder) = encode (popArgs newavails reqret) mx
                  in map (decodeVarsPos ixs) $ -- applies the new Var Names (X n)
                     map (\ (exprs, sub, m) -> (exprs, retrieve decoder sub `plusSubst` subst, mx+m)) -- apply PS input(?)
                     (unMx (lmt mt tn) !! d) :: [Possibility CoreExpr]
                       | annAvs <- combs (d+1) annAvails,
                         let (ixs, newavails) = unzip annAvs
               ] :: [Possibility CoreExpr])

{- {{{ IORef cnts

{-# NOINLINE luFunShcnt #-}
luFunShcnt :: IORef Int
luFunShcnt  = unsafePerformIO (newIORef 0)
cntluFunShcnt  :: IO ()
cntluFunShcnt = do
  modifyIORef luFunShcnt (+1)
  val <- readIORef luFunShcnt
  putStrLn $ "luFunShcnt = " ++ show val

{-# NOINLINE pscnt #-}
pscnt :: IORef Int
pscnt  = unsafePerformIO (newIORef 0)
cntpscnt  :: IO ()
cntpscnt = do
  modifyIORef pscnt (+1)
  val <- readIORef pscnt
  putStrLn $ "pscnt = " ++ show val

{-# NOINLINE dcnt #-}
dcnt :: IORef Int
dcnt  = unsafePerformIO (newIORef 0)
cntdcnt  :: IO ()
cntdcnt = do
  modifyIORef dcnt (+1)
  val <- readIORef dcnt
  putStrLn $ "dcnt = " ++ show val

}}} -}

{- {{{ wasn't used
lookupFunsPoly :: (Search m, Expression e) => Generator m e -> Generator m e
lookupFunsPoly behalf memodeb@(_,mt,_,cmn) avail reqret
    = PS (\subst mx ->
              let (tn, decoder) = encode (popArgs avail reqret) mx
              in ifDepth (<= memodepth (opt cmn))
                         (fmap (\ (exprs, sub, m) -> (exprs, retrieve decoder sub `plusSubst` subst, mx+m)) $ fromMemo $ lmt mt tn)
                         (unPS (behalf memodeb avail reqret) subst mx) )
}}} -}

instance WithCommon ProgGen where
    extractCommon (PG (_,_,_,cmn)) = cmn
instance ProgramGenerator ProgGen where
    mkTrie = mkTriePG
    unifyingPrograms ty (PG md@(_,_,_,cmn)) = trace ("Starting search of functions with type: " ++ show ty ++ "\n") $
      fromRc $ fmap (toAnnExpr $ reducer cmn) $ -- fmap (\ce -> trace ("Current CE: " ++ show ce) ce) $
      catBags $
      unifyingPossibilities ty md
instance ProgramGeneratorIO ProgGen where
    mkTrieIO cmn classes tces = return $ mkTriePG cmn classes tces
    unifyingProgramsIO ty (PG x@(_,_,_,cmn)) = fmap (toAnnExpr $ reducer cmn) $ catBags $ (\ (es,_,_) -> es) <$> unifyingPossibilitiesIO ty x

--unifyingPossibilities :: Search m => Type -> MemoDeb (ClassLib CoreExpr) CoreExpr -> m [CoreExpr]
unifyingPossibilities :: Type -> MemoDeb (ClassLib CoreExpr) CoreExpr -> Recomp [CoreExpr]
unifyingPossibilities ty memodeb =
  runPS (mguProgs memodeb [] ty)

unifyingPossibilitiesIO :: Type -> MemoDeb (ClassLib CoreExpr) CoreExpr -> RecompT IO ([CoreExpr],Subst,TyVar)
unifyingPossibilitiesIO ty memodeb = unPS (mguProgsIO memodeb [] ty) emptySubst 0

-- newtype ProgGen = PG (MemoDeb (ClassLib CoreExpr) CoreExpr) -- ^ internal data representation
-- newtype ClassLib e = CL (MemoDeb (ClassLib e) e)
type MemoDeb c a = (c, MemoTrie a, ([[Prim]],[[Prim]]), Common)


mkTriePG :: Common -> [Typed [CoreExpr]] -> [[Typed [CoreExpr]]] -> ProgGen
-- mkTriePG cmn [] [[typed prims]]
mkTriePG cmn classes tces =   let qtl = splitPrimss tces :: ([[Prim]], [[Prim]])
                                  trie = mkTrieMD qtl cmn
                              in PG trie
mkCL :: Common -> [Typed [CoreExpr]] -> ClassLib CoreExpr
mkCL cmn classes | trace (show classes) False = undefined
mkCL cmn classes = CL $ mkTrieMD ([],[map annotateTCEs classes]) cmn

mkTrieMD :: ([[Prim]],[[Prim]]) -> Common -> MemoDeb (ClassLib CoreExpr) CoreExpr
mkTrieMD qtl cmn
    = let trie :: MapType (Matrix (Possibility CoreExpr))
          trie = mkMT (tcl cmn)
                      (\ty -> fromRc
                        (let (avail,t) = splitArgs ty
                          in freezePS (length avail) ty (mguFuns memoDeb avail t))) -- :: Type -> Matrix (Possibility CoreExpr)
          memoDeb = (undefined,trie,qtl,cmn)
       in memoDeb

-- moved from DebMT.lhs to avoid cyclic modules.
freezePS :: Search m => Int -> Type -> PriorSubsts m (Bag CoreExpr) -> m (Possibility CoreExpr)
freezePS arity ty ps
    = let mxty = maxVarID ty -- `max` maximum (map maxVarID avail)
      in mergesortDepthWithBy (\(xs,k,i) (ys,_,_) -> (xs `mappend` ys, k, i)) (\(_,k,_) (_,l,_) -> k `compare` l) $ fps arity mxty ps
fps :: Search m => Int -> TyVar -> PriorSubsts m [CoreExpr] -> m ([CoreExpr],[(TyVar, Type)],TyVar)
fps arity mxty (PS f) = do
                     (exprs, sub, m) <- f emptySubst (mxty+1)
                     let es = filter (not . isAbsent arity) exprs
                     guard $ not $ length es `seq` null es
                     return (es, filterSubst sub mxty, m)
    where filterSubst :: Subst -> TyVar -> [(TyVar, Type)]
          filterSubst sub  mx = [ t | t@(i,_) <- sub, inRange (0,mx) i ] -- note that the assoc list is NOT sorted.


type Generator m e = MemoDeb (ClassLib e) e -> [Type] -> Type -> PriorSubsts m [e]

mguProgramsIO, mguProgsIO :: Generator (RecompT IO) CoreExpr

mguProgramsIO memodeb = applyDo (mguProgsIO memodeb)

mguProgsIO memodeb@(_,mt,_,cmn) = wind (>>= (return . fmap Lambda)) (\avail reqret -> reorganize (\newavail -> (\memodeb avail reqr -> memoPSRTIO (memoCond $ opt cmn) -- (\_ty _dep -> return (Disk "/tmp/memo/mlist")  {- とりあえずこれでテスト -})
                 mt
                 (\ty -> let (av,rr) = splitArgs ty in generateFuns mguProgramsIO memodeb av rr)
                 (popArgs avail reqr)) memodeb newavail reqret) avail)



mguPrograms, mguProgs :: (Search m) => Generator m CoreExpr
mguFuns :: (Search m) => Generator m CoreExpr

mguPrograms memodeb = applyDo (mguProgs memodeb)

mguProgs memodeb =
  wind (>>= (return . fmap (mapCE Lambda))) (lookupFunsShared memodeb)

-- {{{
--mguProgs memodeb = wind (>>= (return . fmap Lambda)) (\avail reqret -> reorganize (\newavail -> lookupFunsPoly mguFuns memodeb newavail reqret) avail)
{- どっちがわかりやすいかは不明
mguProgs memodeb avail (t0:->t1) = do result <- mguProgs memodeb (t0 : avail) t1
                                      return (fmap Lambda result)
mguProgs memodeb avail reqret = reorganize (\newavail -> lookupFunsPoly mguFuns memodeb newavail reqret) avail
}}} -}

mguFuns = generateFuns mguPrograms

-- MemoDebの型が違うと使えない．
generateFuns :: (Search m) =>
                Generator m CoreExpr                               -- ^ recursive call
                -> Generator m CoreExpr
generateFuns rec memodeb@(_, _, (primgen,primmono),cmn) avail reqret
    = let -- clbehalf  =  mguPrograms classLibmemodeb []
          behalf    = rec memodeb avail
          -- lltbehalf = lookupListrie (opt cmn) rec memodeb avail -- heuristic filtration
          lenavails = genericLength avail
--          fe :: Type -> Type -> [CoreExpr] -> [CoreExpr] -- ^ heuristic filtration
          fe        = filtExprs (guess $ opt cmn)
          rg -- | (trace (show (tv0 $ opt cmn, tv1 $ opt cmn)) False) = undefined
               | tv0 $ opt cmn = retGenTV0
               | tv1 $ opt cmn = retGenTV1
               | otherwise = retGen
      in fromAssumptions cmn lenavails behalf mguPS reqret avail `mplus`
         mapSum (rg cmn lenavails fe undefined undefined behalf reqret) primgen `mplus`
         mapSum (retPrimMono cmn lenavails undefined undefined behalf mguPS reqret) primmono

{- {{{
lookupListrie opt _ _ _ _ | trace (show $ guess opt) False = undefined
lookupListrie opt rec memodeb avail t
--                      | constrL opt = mguAssumptions t avail
                      | guess opt = do args <- rec memodeb avail t
                                       let args' = filter (not.isClosed.toCE) args
                                       when (null args') mzero
                                       return args'
                      | otherwise = do args <- rec memodeb avail t
                                       let args' = filter (not.isConstrExpr.toCE) args
                                       when (null args') mzero
                                       return args'
}}} -}
