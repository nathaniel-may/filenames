{-# LANGUAGE GADTs, LambdaCase #-}

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
typecheck_ _ (RootU []) = Left EmptySourceFile

-- program root is defined by assignment to value "format"
typecheck_ table (RootU (expr : exprs)) = do
    (table1, exprt) <- typecheck_ table expr
    tablesNExprts <- traverse (typecheck_ table) exprs
    let (tables, exprts) = unzip tablesNExprts
    -- all top-level definitions must be assignments. Should be enforced at parse time so this is here just in case.
    types <- traverse inferType (exprt : exprts)
    let mismatches = filter (/=UnitTag) types
    _ <- case mismatches of
        []  -> Right ()
        -- only reporting first mismatch if there are several. could change the exception to show them all.
        (got : _) -> Left $ TopLevelNotAssignment got
    let finalTable = foldr M.union table1 tables
    finalExprt <- maybeToRight FormatNotFound (M.lookup (Name "format") finalTable)
    pure (finalTable, finalExprt)

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
    let mismatches = filter (/=expectedType) inferred
    case mismatches of
        [] -> Right (table, ListT expectedType $ snd <$> checked)
        (got : _) -> Left (ListTypeMismatch expectedType got)

typecheck_ table (AssignmentU name v) = do
    (table2, exprt) <- typecheck_ table v
    pure (M.insert name exprt table2, UnitT)

typecheck_ table (IdentifierU name) = do
    exprt <- maybeToRight (NoValueNamed name) (M.lookup name table)
    pure (table, exprt)


checkType :: Type -> ExprT -> Either TypeException ()
checkType UnitTag UnitT = Right ()
checkType UnitTag got = Left $ TypeMismatch UnitTag (rightToMaybe $ inferType got)
checkType StringTag (StringT _) = Right ()
checkType StringTag got = Left $ TypeMismatch StringTag (rightToMaybe $ inferType got)
checkType IntTag (IntT _) = Right ()
checkType IntTag got = Left $ TypeMismatch IntTag (rightToMaybe $ inferType got)
checkType BoolTag (BoolT _) = Right ()
checkType BoolTag got = Left $ TypeMismatch BoolTag (rightToMaybe $ inferType got)
checkType expected@(ListTag t) (ListT t' xs) = 
    if t == t'
    then Left $ TypeMismatch expected (Just $ ListTag t')
    else mapM_ (checkType t) xs
checkType expected@(ListTag _) got = Left $ TypeMismatch expected (rightToMaybe $ inferType got)

inferType' :: (a, ExprT) -> Either TypeException Type
inferType' = inferType . snd

inferType :: ExprT -> Either TypeException Type
inferType UnitT = Right UnitTag
inferType (StringT _) = Right StringTag
inferType (IntT _) = Right IntTag
inferType (BoolT _) = Right BoolTag
inferType (ListT tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)
