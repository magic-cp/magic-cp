{-# LANGUAGE CPP, ExistentialQuantification, RankNTypes, TemplateHaskell #-}

import Data.Int

import qualified Language.Haskell.TH as TH

-- Types.hs


type TyVar = Int8
type TyCon = TyVar -- TyCon should be the same or bigger than TyVar, because the quantify function converts TyVar into TyCon
data Type = TV {-# UNPACK #-} !TyVar | TC {-# UNPACK #-} !TyCon | TA Type Type | Type :> Type | Type :-> Type | Type :=> Type

type Subst = [(TyVar,Type)]


-- DebMT.lhs

type Possibility e = (Bag e, Subst, TyVar)
data MapType a
    = MT  {
           tvMT   :: [a],
           tcMT   :: [a],
           genMT  :: [a], -- "forall" stuff
           taMT   :: MapType (MapType a),
           funMT  :: MapType (MapType a)
          }

-- Combinatorial.hs

type Stream a = [a]
type Bag a = [a]

newtype Matrix a = Mx {unMx::Stream (Bag a)} deriving Show

newtype Recomp a = Rc {unRc::Int->Bag a}



-- ProgramGenerator.hs

type Prim = (Int, Int, Type, TyVar, Typed [CoreExpr])

-- ProgGen.hs

type BF = Recomp

type BFM = Matrix

type MemoTrie a = MapType (BFM (Possibility a))


newtype ProgGen = PG (MemoDeb (ClassLib CoreExpr) CoreExpr) -- ^ internal data representation
newtype ClassLib e = CL (MemoDeb (ClassLib e) e)
-- ClassLib (CoreExpr) = CL (MemoDeb (ClassLib CoreExpr) CoreExpr)
type MemoDeb c a = (c, MemoTrie a, ([[Prim]],[[Prim]]), Common)




-- CoreLang.hs

type Primitive = (HValue, TH.Exp, TH.Type)
newtype HValue = HV (forall a. a)


data CoreExpr = S | K | I | B | C | S' | B' | C' | Y
                | Lambda CoreExpr | X {-# UNPACK #-} !Int8 -- de Bruijn notation
                | FunLambda CoreExpr | FunX Int8 -- different system of de Bruijn notation for functions, used by IOPairs.hs
                | Tuple {-# UNPACK #-} !Int8
                | Primitive {primId :: {-# UNPACK #-} !Var}  -- (This should be Var instead of Int8 because the number space is being exhausted!)
                | PrimCon   {primId :: {-# UNPACK #-} !Var}  -- the primitive is a constructor expression
                | Context Dictionary
                | CoreExpr :$ CoreExpr
                | Case CoreExpr [(Var,Int8,CoreExpr)] -- the case expression. [(primitive ID of the constructor, arity of the constructor, rhs of ->)]
                | Fix  CoreExpr Int8 [Int8]            -- Fix expr n is === foldl (:$) (Y :$ FunLambda (napply n Lambda expr)) (map X is)
                | VarName String -- This is only used for pretty printing IOPairs.Expr. Use de Bruijn variables for other purposes.
                  deriving (Eq, Show, Ord)

newtype Dictionary = Dict {undict :: Dynamic} deriving (Show)

-- PolyDynamic.hs
data Dynamic = Dynamic {dynType::Type, unsafeFromDyn::forall a. a, dynExp::TH.Exp}






main = return ()
