{-# LANGUAGE GADTs, TupleSections #-}

module TypeChecker where

import           CustomPrelude -- import all
import qualified Data.Map as M
import           Exceptions    (TypeException(..))
import           Types         -- import all



typecheck :: ExprU -> Either TypeException ExprT
typecheck x = do
    (_, exprt) <- typecheck_ M.empty x
    pure exprt

typecheck_ :: ValueTable -> ExprU -> Either TypeException (ValueTable, ExprT)
-- nothing in source file
typecheck_ _ (RootU []) = Left Boop1

-- top-level program is defined by assignment to value "format"
-- TODO this is where I would catch top-level unassigned values. the returned ExprT wouldn't be a UnitT.
typecheck_ table (RootU (expr : exprs)) = do
    (table1, _) <- typecheck_ table expr
    tables <- traverse (fmap fst . typecheck_ table) exprs
    let finalTable = foldr M.union table1 tables
    (finalTable,) <$> maybeToRight Boop1 (M.lookup (Name "format") finalTable)

typecheck_ table (StringU s) =
    Right (table, StringT s)

typecheck_ table (IntU i) =
    Right (table, IntT i)

typecheck_ table (BoolU b) =
    Right (table, BoolT b)

typecheck_ table (ListU []) =
    Right (table, ListT StringTag [])

typecheck_ table (ListU elems@(x : _)) = do
    expectedType <- inferType' =<< typecheck_ table x
    checked <- traverse (typecheck_ table) elems
    inferred <- traverse inferType' checked
    if all (== expectedType) inferred
    then Right (table, ListT expectedType $ snd <$> checked)
    else Left Boop2

typecheck_ table (AssignmentU name v) = do
    (table2, exprt) <- typecheck_ table v
    pure (M.insert name exprt table2, UnitT)

typecheck_ table (IdentifierU name) = do
    exprt <- maybeToRight Boop1 (M.lookup name table)
    pure (table, exprt)


checkType :: Type -> ExprT -> Either TypeException ()
checkType UnitTag UnitT = Right ()
checkType UnitTag _ = Left Boop1
checkType StringTag (StringT _) = Right ()
checkType StringTag _ = Left Boop1
checkType IntTag (IntT _) = Right ()
checkType IntTag _ = Left Boop1
checkType BoolTag (BoolT _) = Right ()
checkType BoolTag _ = Left Boop1
checkType (ListTag t) (ListT t' xs) = 
    if t == t'
    then Left Boop2
    else mapM_ (checkType t) xs
checkType (ListTag _) _ = Left Boop1

inferType' :: (a, ExprT) -> Either TypeException Type
inferType' = inferType . snd

inferType :: ExprT -> Either TypeException Type
inferType UnitT = Right UnitTag
inferType (StringT _) = Right StringTag
inferType (IntT _) = Right IntTag
inferType (BoolT _) = Right BoolTag
inferType (ListT tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)
