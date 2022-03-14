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
    types <- traverse (inferType table) (exprt : exprts)
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


checkType :: ValueTable -> Type -> ExprT -> Either TypeException ()
checkType _ UnitTag UnitT = Right ()
checkType table UnitTag got = Left . TypeMismatch UnitTag =<< inferType table got
checkType _ StringTag (StringT _) = Right ()
checkType table StringTag got = Left . TypeMismatch StringTag =<< inferType table got
checkType _ IntTag (IntT _) = Right ()
checkType table IntTag got = Left . TypeMismatch IntTag =<< inferType table got
checkType _ BoolTag (BoolT _) = Right ()
checkType table BoolTag got = Left . TypeMismatch BoolTag =<< inferType table got
checkType table expected@(ListTag t) (ListT t' xs) = 
    if t == t'
    then Left $ TypeMismatch expected (ListTag t')
    else mapM_ (checkType table t) xs
checkType table expected@(ListTag _) got = Left . TypeMismatch expected =<< inferType table got
checkType table (FnTag _ params) (FnCallT name params') = do
    let lparams = length params
    let lparams' = length params'
    if' (lparams /= lparams') (Left $ TypeMismatchNumFnParams name lparams lparams') (Right ())
    inferred <- traverse (inferType table)  params'
    let mismatches = filter (uncurry (/=)) (params `zip` inferred)
    case mismatches of
        [] -> Right ()
        ((expected, got) : _) -> Left $ TypeMismatchFnParam name expected got
checkType table FnTag{} got = Left . TypeMismatch BoolTag =<< inferType table got

-- TODO remove this.
inferType' :: (ValueTable, ExprT) -> Either TypeException Type
inferType' = uncurry inferType

inferType :: ValueTable -> ExprT -> Either TypeException Type
inferType _ UnitT = Right UnitTag
inferType _ (StringT _) = Right StringTag
inferType _ (IntT _) = Right IntTag
inferType _ (BoolT _) = Right BoolTag
inferType table (ListT tag xs) = mapM_ (checkType table tag) xs >> pure (ListTag tag)
inferType table (FnCallT name _) = do
    exprt <- maybeToRight (NoFunctionNamed name) (M.lookup name table)
    case exprt of
        (FnDefT _ ret params) -> Right $ FnTag ret params
        got -> Left . NotAFunction name =<< inferType table got
inferType _ FnDefT{} = Right UnitTag
