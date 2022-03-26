{-# LANGUAGE GADTs, FlexibleContexts, TupleSections #-}

module TypeChecker where

import           Control.Monad.Except -- import all
import           Control.Monad.Reader -- import all
import           CustomPrelude        -- import all
import qualified Data.Map as M
import           Exceptions           (TypeException(..))
import           Types                -- import all


typecheck :: MonadError TypeException m => ExprU -> m ExprT
typecheck x = snd <$> runReaderT (typecheck_ x) builtins

typecheck_ :: (MonadReader Env m, MonadError TypeException m) => ExprU -> m Closure
-- nothing in source file
typecheck_ (RootU []) = throwError EmptySourceFile

-- program root is defined by assignment to value "format"
typecheck_ (RootU exprs) = do
    closures <- traverse typecheck_ exprs
    types <- traverse inferType closures
    let mismatches = filter (/=UnitTag) types
    -- all top-level definitions must be assignments. Should be enforced here, not at parse time.
    _ <- case mismatches of
        []  -> pure ()
        -- only reporting first mismatch if there are several. could change the exception to show them all.
        (got : _) -> throwError $ TopLevelNotAssignment got
    table <- ask
    let (tables, _) = unzip closures
    let finalTable = foldr M.union table tables
    finalExprt <- maybe (throwError FormatNotFound) pure (M.lookup (Name "format") finalTable)
    pure (finalTable, finalExprt)

typecheck_ (StringU s) =
    asks (, StringT s)

typecheck_ (IntU i) =
    asks (, IntT i)

typecheck_ (BoolU b) =
    asks (, BoolT b)

typecheck_ (ListU []) =
    asks (, ListT StringTag [])

typecheck_ (ListU elems@(x : _)) = do
    table <- ask
    expectedType <- inferType =<< typecheck_ x
    checked <- traverse typecheck_ elems
    inferred <- traverse inferType checked
    let mismatches = filter (/=expectedType) inferred
    case mismatches of
        [] -> pure (table, ListT expectedType $ snd <$> checked)
        (got : _) -> throwError (ListTypeMismatch expectedType got)

typecheck_ (AssignmentU name v) = do
    (table2, exprt) <- typecheck_ v
    -- TODO this overwrites assignments. should check if it exists first
    pure (M.insert name exprt table2, UnitT)

typecheck_ (IdentifierU name) = do
    table <- ask
    maybe (throwError $ NoValueNamed name) (pure . (table,)) (M.lookup name table)

typecheck_ (FnCallU name params) = do
    table <- ask
    exprt <- maybe (throwError $ NoValueNamed name) pure (M.lookup name table)
    case exprt of
        (FnDefT _ ret' params') -> do
            -- throws away the tables from param typechecking
            -- no additional definitions should be added there
            checked <- traverse typecheck_ params
            inferred <- traverse inferType checked
            let mismatches = filter (uncurry (/=)) (params' `zip` inferred)
            let (_, checkedExprs) = unzip checked
            case mismatches of
                [] -> pure (table, FnCallT name ret' checkedExprs)
                ((expected, got) : _) -> throwError $ TypeMismatchFnParam name expected got
        got -> throwError . NotAFunction name =<< inferType (table, got)


checkType :: (MonadReader Env m, MonadError TypeException m) => Closure -> Type -> m ()
checkType (_, UnitT) UnitTag = pure ()
checkType closure UnitTag = throwError . TypeMismatch UnitTag =<< inferType closure
checkType (_, StringT _) StringTag = pure ()
checkType closure StringTag = throwError . TypeMismatch StringTag =<< inferType closure
checkType (_, IntT _) IntTag = pure ()
checkType closure IntTag = throwError . TypeMismatch IntTag =<< inferType closure
checkType (_, BoolT _) BoolTag = pure ()
checkType closure BoolTag = throwError . TypeMismatch BoolTag =<< inferType closure
checkType (table, ListT t' xs) expected@(ListTag t) = 
    if t /= t'
    then throwError $ TypeMismatch expected (ListTag t')
    else traverse_ (\x -> checkType (table, x) t) xs
checkType closure expected@(ListTag _) = throwError . TypeMismatch expected =<< inferType closure
checkType (table, FnCallT name ret' params') (FnTag ret params) = do
    if ret /= ret' then throwError $ TypeMismatchFnReturn name ret ret' else pure ()
    let lparams = length params
    let lparams' = length params'
    if' (lparams /= lparams') (throwError $ TypeMismatchNumFnParams name lparams lparams') (pure ())
    inferred <- traverse (curry inferType table) params'
    let mismatches = filter (uncurry (/=)) (params `zip` inferred)
    case mismatches of
        [] -> pure ()
        ((expected, got) : _) -> throwError $ TypeMismatchFnParam name expected got
checkType closure FnTag{} = throwError . TypeMismatch BoolTag =<< inferType closure


inferType :: (MonadReader Env m, MonadError TypeException m) => Closure -> m Type
inferType (_, UnitT) = pure UnitTag
inferType (_, StringT _) = pure StringTag
inferType (_, IntT _) = pure IntTag
inferType (_, BoolT _) = pure BoolTag
inferType closure@(_, ListT tag _) = checkType closure (ListTag tag) $> ListTag tag
inferType (table, FnCallT _ ret params) = FnTag ret <$> traverse (curry inferType table) params
inferType (_, FnDefT{}) = pure UnitTag


builtins :: Env
builtins = M.fromList [
    (Name "<",   FnDefT (Name "<")  BoolTag [IntTag, IntTag])
  , (Name ">",   FnDefT (Name ">")  BoolTag [IntTag, IntTag])
  , (Name "==",  FnDefT (Name "==") BoolTag [IntTag, IntTag])
  , (Name "<=",  FnDefT (Name "<=") BoolTag [IntTag, IntTag])
  , (Name ">=",  FnDefT (Name ">=") BoolTag [IntTag, IntTag])
  ]
