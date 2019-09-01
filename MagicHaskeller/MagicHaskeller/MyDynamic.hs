--
-- (c) Susumu Katayama
--
{-# LANGUAGE CPP #-}
# ifdef REALDYNAMIC
module MagicHaskeller.MyDynamic(module MagicHaskeller.PolyDynamic) where
import MagicHaskeller.PolyDynamic
# else
module MagicHaskeller.MyDynamic(module MagicHaskeller.FakeDynamic) where
import MagicHaskeller.FakeDynamic -- MY dynamic
# endif
