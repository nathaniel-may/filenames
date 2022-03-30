{-# LANGUAGE BangPatterns #-}

module Optimizer where

import CustomPrelude -- import all
import Types         -- import all


-- TODO debugging this sucks. maybe I should have errors?

optimize :: ExprT -> ExprO
optimize UnitT = UnitO
optimize (IntT x) = IntO x
optimize (StringT x) = StringO x
optimize (BoolT x) = BoolO x
optimize (ListT _ xs) = ListO (optimize <$> xs)
optimize (FnT name _ xs) = FnO name (optimize <$> xs)
optimize x@FlipT{} = optimize . partial $ x
optimize x@ApplyT{} = optimize . partial $ x

-- apply partial evaluation
partial :: ExprT -> ExprT
partial (FlipT (FlipT x)) = x
partial (FlipT (FnT name tag (x : y : z))) = FnT name tag (partial <$> (y : x : z))
partial (ApplyT (FlipT (FnT name (FnTag _ ret) xs)) x) = FlipT (FnT name ret (partial <$> (xs <> [x])))
partial (ApplyT (FnT name tag xs) x) = FnT name tag (partial <$> (xs <> [x]))
partial (ApplyT x y) = ApplyT (partial x) (partial y)
partial x = x
